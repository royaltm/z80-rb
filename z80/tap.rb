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
			raise TapeError, "Name should cointain ASCII 7-bit only!" unless name.ascii_only?
			head = "\x00\x03" + name.ljust(10)[0,10] + [code.bytesize, org, 0x8000].pack('v3')
			TAP.addsum head
			data =  "\xff" + code
			TAP.addsum data
			[head.bytesize].pack('v') + head + [data.bytesize].pack('v') + data
		end
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
				tap = File.open(file, 'rb') {|f| f.read}
				index = args[:index].to_i
				btype = length = name = nil
				begin
					size, tap = tap.unpack('va*')
					chunk, tap = tap.unpack("a#{size}a*")
					raise TapeError, "Invalid TAP file checksum: `#{file}'." unless cksum chunk
					raise TapeError, "Invalid TAP block too short: `#{file}'." unless chunk.bytesize == size
					type, data, _ = chunk.unpack("Ca#{size-2}C")
					case type
					when 0x00
						raise TapeError, "Invalid TAP header length: `#{file}'." unless data.bytesize == 17
						btype, name, length = data.unpack('CA10v')
						next
					when 0xff
						btype = length = name = nil unless index == 0
					else
						raise TapeError, "Invalid TAP file chunk: `#{file}'."
					end
					index-=1
				end while index >= 0
				if length
					raise TapeError, "Invalid TAP bytes length: `#{file}'." unless data.bytesize == length
					$stderr.puts "Importing: `#{file}': (#{name})"
				else
					$stderr.puts "Importing: `#{file}': headerless chunk"
				end
				data
			end
		end
	end
end
