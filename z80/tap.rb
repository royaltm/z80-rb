# -*- coding: BINARY -*-
module Z80
	##
	#  ==Adds TAP format support to *program*.
	#  Inside your *program* add:
	#    include Z80::TAP
	#  and Program.import_file will make use of it.
	#
	#  Pass additional +:index+ => +n+ argument to Program.import_file to choose n'th bytes block from tap file. 
	#
	#  Instances of your program will receive two additional methods:
	#  * #save_tap
	#  * #to_tap
	#
	module TAP
		class TapeError < StandardError; end
		TYPE_PROGRAM = 0
		TYPE_NUMBER_ARRAY = 1
		TYPE_CHAR_ARRAY = 2
		TYPE_CODE = 3
		##
		#  Saves Program#code in TAP file.
		#
		#  * +file+    specifies filename.
		#  * +options+
		#    * +:name+ can contain max 10 ascii (7-bit) characters.
		#      If not given base name of +file+ will be used instead.
		#    * +:append+ if true code will be appended to tap.
		def save_tap(file, options = {})
			options = {
				:name => nil,
				:append => false
			}.update options
			name = options[:name] || File.basename(file, '.tap')
			name+= '.tap' unless File.extname(name).downcase == '.tap'
			File.open(name, options[:append] ? 'ab' : 'wb') {|f| f.write to_tap(File.basename name, '.tap') }
		end
		##
		#  Wraps Program#code inside TAP format.
		#
		#  * +name+ can contain max 10 ascii (7-bit) characters.
		#
		def to_tap(name)
			HeaderBody.new(
				Header.new(TYPE_CODE, name, code.bytesize, org, 0x8000),
				Body.new(code)
			).to_tap
		end

		##
		#  A struct which represents a header and a body chunk of a TAP file.
		#
		#  Has properties:
		#
		#  * +header+ as Header or +nil+
		#  * +body+ as Body
		#
		#  HeaderBody#to_tap converts struct instance to TAP blob.
		HeaderBody = ::Struct.new :header, :body do
			def to_tap
				res = ""
				res << header.to_tap unless header.nil?
				res << body.to_tap(header && header.length) unless body.nil? 
				res
			end
			def is_program?
				header.is_program?
			end
			def is_code?
				header.is_code?
			end
		end

		##
		#  A struct which represents TAP header chunk.
		#
		#  Has properties:
		#
		#  * +type+ - 0 - Program, 1 - Number array, 2 - Character array, 3 - Code
		#  * +name+ as String
		#  * +length+ as Integer
		#  * +p1+ as Integer
		#  * +p2+ as Integer
		Header = ::Struct.new :type, :name, :length, :p1, :p2 do
			def is_program?
				type == TYPE_PROGRAM
			end
			def is_code?
				type == TYPE_CODE
			end
			def is_screen?
				is_code? and length == 6912 and p1 == 16384
			end
			def to_tap
				raise TapeError, "Name should cointain ASCII 7-bit only!" unless name.ascii_only?
				head = [0, type].pack('CC') + name.ljust(10)[0,10] + [length, p1, p2].pack('v3')
				TAP.addsum head
				[head.bytesize].pack('v') + head
			end
		end

		##
		#  A struct which represents TAP body chunk.
		#  Has +data+ as String property.
		Body = ::Struct.new :data do
			def to_tap(length=nil)
				raise TapeError, "Header length dosn't match" unless length.nil? || data.bytesize == length
				body = "\xff" + data
				TAP.addsum body
				[body.bytesize].pack('v') + body
			end
		end

		##
		#  TAP tools
		#
		class << self
			def addsum(s) # :nodoc:
				s << s.split('').map(&:ord).inject(&:^).chr
			end
			def cksum(s) # :nodoc:
				s.split('').map(&:ord).inject(&:^) == 0
			end

			##
			#  Program.import_file uses this method to read from TAP file.
			#
			def read_data(file, args = {})
				Tap.read_header_body(file, args) do |chunk|
					if chunk.header
						$stderr.puts "Importing: `#{file}': (#{chunk.header.name})"
					else
						$stderr.puts "Importing: `#{file}': headerless chunk"
					end
					return chunk.body.data
				end
				raise "Index: #{index} not found in TAP file: `#{file}`"
			end

			##
			#  Reads HeaderBody from TAP file.
			#
			#  Pass additional +:index+ => +n+ argument to choose n'th chunk from file. 
			#
			#  Pass a block to visit a chunk.
			#
			def read_chunk(file, args = {})
				tap = File.open(file, 'rb') {|f| f.read}
				index = args[:index].to_i
				TAP.parse_tap(tap, file).each_with_index do |chunk, i|
					if i == index
						if block_given?
							yield chunk
						else
							return chunk
						end
					end
				end
			end

			##
			#  Returns an Enumerator of HeaderBody structs which describes blocks found in tap blob.
			#  Optionally unwraps TZX headers.
			#
			#  Pass a block to visit each chunk.
			#  Optionally pass file name for error messages.
			#
			def parse_tap(tap, file='-', &block)
				tap, is_tzx = TAP.unpack_from_tzx_header tap, file
				enu = ::Enumerator.new do |y|
					header = nil
					loop do
						if is_tzx
							tap = TAP.unpack_from_tzx_chunk tap, file
						end
						size, tap = tap.unpack('va*')
						break if size.nil? && tap.empty?
						chunk, tap = tap.unpack("a#{size}a*")
						raise TapeError, "Invalid TAP file checksum: `#{file}'." unless cksum(chunk)
						raise TapeError, "Invalid TAP block too short: `#{file}'." unless chunk.bytesize == size
						type, data, _cksum = chunk.unpack("Ca#{size-2}C")
						case type
						when 0x00
							raise TapeError, "Invalid TAP header length: `#{file}'." unless data.bytesize == 17
							header = Header.new(*data.unpack('CA10v3'))
						when 0xff
							unless header.nil?
								raise TapeError, "Invalid TAP bytes length: `#{file}'." unless data.bytesize == header.length
							end
							chunk = HeaderBody.new(header, Body.new(data))
							header = nil
							y << chunk
						else
							raise TapeError, "Invalid TAP file chunk: `#{file}'."
						end
					end
				end
				if block_given?
					enu.each(&block)
				else
					enu
				end
			end

			def unpack_from_tzx_header(tap, file='-') # :nodoc:
				if tap.start_with?("ZXTape!\x1A") && tap.bytesize > 13
					_signature, major, _minor, tap = tap.unpack("a8CCa*")
					# $stderr.puts "unpacking TZX header: #{major}.#{_minor}"
					raise "Unknown TZX major: `#{file}'" unless major == 1
					[tap, true]
				else
					[tap, false]
				end
			end

			def unpack_from_tzx_chunk(tap, file='-') # :nodoc:
				if tap.bytesize > 3
					id, tap = tap.unpack("Ca*")
					# $stderr.puts "unpacking TZX: id=0x#{id.to_s(16)}"
					case id
					when 0x10
						_wait, tap = tap.unpack("va*")
					when 0x35
						_name, size, tap = tap.unpack("a10L<a*")
						_info, tap = tap.unpack("a#{size}a*")
					when 0x5A
						_glue, tap = tap.unpack("a9a*")
					else
						raise "Only the standard speed data block are currently handled in TZX files, got: 0x#{id.to_s(16)} in `#{file}'"
					end
				end
				tap
			end
		end
	end
end
