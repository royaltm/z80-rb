# -*- coding: BINARY -*-
require 'z80'
require 'zxlib/math' unless defined?(ZXLib::Math)
##
#  A module with ZX Spectrum's BASIC program utilities.
#
#  See: Basic::Program, Basic::Vars, Basic::Variable
#
#  Example:
#
#    require 'zxlib/basic'
#
#    program = Basic.parse_source <<-EOB
#      10 LET a$="Hello World!"
#      20 PRINT a$
#    EOB
#    program.start = 10
#    program.save_tap 'helloworld.tap'
#
#    program = Basic.parse_source '10 PRINT s$'
#    program.start = 10
#    program.vars << Basic::Variable.new_string('s$', '`FLASH 1`""`INK 1`Hello World!`INK 0`""`FLASH 0`')
#    program.save_tap 'helloworld.tap', append: true
#    puts program.vars
#
#    chunk = Z80::TAP.read_chunk('helloworld.tap')
#    program = Basic.from_tap_chunk(chunk)
#    puts program
#    source = program.to_source
#
module Basic
	##
	#  Represents a ZX Basic program in a semi-parsed form.
	class Program
		include Z80::TAP

		# An array of Basic::Line instances representing this Basic program body.
		attr_accessor :lines
		# The optional starting line of a Basic program as an integer.
		attr_accessor :start
		# A Basic::Vars instance with a program's run-time variables.
		attr_reader :vars

		def initialize(lines, vars = nil, start = nil)
			@lines = lines
			@vars = Vars.new(vars)
			@start = start
		end
		##
		#  Creates the textual representation of a ZX Basic::Program.
		#
		#  Returns an UTF-8 encoded string.
		#
		#  The conversion is done as follows:
		#
		#  * Each character in the ASCII printable range 32..126 except the £ (pound, code 96) is left unmodified.
		#  * The £ (pound, code 96) and the © (code 127) characters are converted to a U+00A3 and U+00A9 accordingly.
		#  * Raw FP numbers beginning with a character code 14 are being stripped outside of literal strings.
		#  * A comma control character (code 6) is encoded as \\t (TABULATION U+0009) character.
		#  * Control characters 8..11 are encoded as Unicode ARROWS (see table below).
		#  * The remaining control characters in the code range 0..31 are encoded using escape sequences.
		#  * The block characters in the code range 128..143 are converted to Unicode BLOCK elements (see table below).
		#  * The characters in the UDG code range 144..164 are converted to CIRCLED LATIN CAPITAL LETTERs.
		#  * Keywords in the code range 165..255 are either encoded as escaped keywords (e.g. `PRINT`) when found inside
		#    literal strings or just as sequences of its constituent characters.
		#
		#  ====Note:
		#  The last rule may lead to some disambiguities. Consider a line:
		#
		#      PRINT RND
		#
		#  The +RND+ keyword in this case may be a variable name consisting of 3 capital letters R N D or a +RND+ function.
		#  The ZX Basic knows the difference because the keyword +RND+ is encoded as a single code point: 165.
		#  However when presented as text you can't really tell the difference.
		#  This may lead to errors when trying to parse such a text back to the ZX Spectrum's binary program format.
		#
		#  To desambiguate keywords from regular characters in strings they are being encoded as escape sequences,
		#  e.g. `GO SUB`, `RND`, `OPEN #`.
		#  Pass +true+ to +:escape_keywords+ option to enforce keywords to be always escaped.
		#
		#  Escape sequences are using GRAVE ACCENT ` (U+0060, also known as a backtick) as enclosing character
		#  because it's absent in the ZX Spectrum's character set.
		#
		#  The control characters are encoded as decimal code numbers, e.g: `12`. More codes can be put inside
		#  an escape sequence using whitespaces or commas as separators, e.g.: `0xff, 201, 0b01010001` stands for 3 bytes.
		#  Any ruby number literal is accepted: decimal, hexadecimal, octal, binary.
		#
		#  Color and cursor position control condes are multi-byte. There are special control escape sequences for them:
		#
		#      code seq. count  special escape sequence format
		#      `16 n`    2      `INK n`
		#      `17 n`    2      `PAPER n`
		#      `18 n`    2      `FLASH n`
		#      `19 n`    2      `BRIGHT n`
		#      `20 n`    2      `INVERSE n`
		#      `21 n`    2      `OVER n`
		#      `22 y x`  3      `AT y,x`
		#      `23 x x`  3      `TAB x`
		#
		#  Where +n+, +x+, +y+ are decimal numbers representing the following character code and at the same time
		#  special control arguments.
		#
		#  Non-ASCII characters and alternative escape sequences:
		#
		#       code  escaped  unicode   description
		#          8  `<`      U+2190 ←  move left
		#          9  `>`      U+2192 →  move right
		#         10  `v`      U+2193 ↓  move down
		#         11  `^`      U+2191 ↑  move up
		#         96  `&`      U+00A3 £  a pound sign
		#        127  `(c)`    U+00A9 ©  a copyright sign
		#        128  `|8`     U+2591 ░  various block characters
		#        129  `|1`     U+259D ▝  
		#        130  `|2`     U+2598 ▘  
		#        131  `|3`     U+2580 ▀  
		#        132  `|4`     U+2597 ▗  
		#        133  `|5`     U+2590 ▐  
		#        134  `|6`     U+259A ▚  
		#        135  `|7`     U+259C ▜  
		#        136  `#7`     U+2596 ▖  
		#        137  `#6`     U+259E ▞  
		#        138  `#5`     U+258C ▌  
		#        139  `#4`     U+259B ▛  
		#        140  `#3`     U+2584 ▄  
		#        141  `#2`     U+259F ▟  
		#        142  `#1`     U+2599 ▙  
		#        143  `#8`     U+2588 █  
		#        144  `a`      U+24B6 Ⓐ  user defined graphics
		#        145  `b`      U+24B7 Ⓑ  
		#        146  `c`      U+24B8 Ⓒ  
		#        147  `d`      U+24B9 Ⓓ  
		#        148  `e`      U+24BA Ⓔ  
		#        149  `f`      U+24BB Ⓕ  
		#        150  `g`      U+24BC Ⓖ  
		#        151  `h`      U+24BD Ⓗ  
		#        152  `i`      U+24BE Ⓘ  
		#        153  `j`      U+24BF Ⓙ  
		#        154  `k`      U+24C0 Ⓚ  
		#        155  `l`      U+24C1 Ⓛ  
		#        156  `m`      U+24C2 Ⓜ  
		#        157  `n`      U+24C3 Ⓝ  
		#        158  `o`      U+24C4 Ⓞ  
		#        159  `p`      U+24C5 Ⓟ  
		#        160  `q`      U+24C6 Ⓠ  
		#        161  `r`      U+24C7 Ⓡ  
		#        162  `s`      U+24C8 Ⓢ  
		#        163  `t`      U+24C9 Ⓣ  
		#        164  `u`      U+24CA Ⓤ  
		#
		#  The above escape sequences may be safely concatenated between a single pair of enclosing backticks, e.g.:
		#
		#        Unicode:    "£©░█ⒶⒷⒸⓊ←→↑↓"
		#        Ascii only: "`&(c)|8#8abcu<>^v`" 
		#
		#  Passing +:ascii_only+ as +true+ will render escape sequences instead of non-ascii characters.
		def to_source(escape_keywords:false, ascii_only:false)
			lines.map { |line| line.to_s escape_keywords: escape_keywords, ascii_only: ascii_only }.join("\n")
		end
		alias_method :to_s, :to_source
		##
		#  Returns index in +lines+ of a Basic line number equal or greater than +line_no+.
		def line_index(line_no)
			lines.index{|l| l.line_no >= line_no}
		end
		##
		#  Returns a new Basic::Program instance with the subset of its lines according to the +line_no+ argument.
		#
		#  +line_no+ may be an integer or a Range. The integer indicates the first line to be included.
		#  The Range selects a range of lines to be included.
		#  +line_no+ relates to the Basic line number.
		def list(line_no)
			if Integer === line_no
				if index = line_index(line_no)
					Program.new lines[index..-1], vars
				else
					Program.new [], vars
				end
			elsif line_no.respond_to? :===
				Program.new lines.select{|l| line_no === l.line_no}, vars
			end
		end
		##
		#  Returns a Basic::Line at +index+ or an array of lines if +Range+ is given.
		def [](index)
			lines[index]
		end
		##
		#  Returns the raw byte representation of the whole ZX Basic program as a binary string.
		def code
			res = ''
			lines.each do |line|
				body = line.body
				res << [line.line_no, body.bytesize + 1, body, 13].pack('S>S<a*C')
			end
			res
		end
		##
		#  Creates a Z80::TAP::HeaderBody instance from Basic::Program#code.
		#
		#  This method is provided for the included Z80::TAP#to_tap and Z80::TAP#save_tap methods.
		def to_tap_chunk(name, line:nil)
			prog_length = code.bytesize
			line ||= start
			Z80::TAP::HeaderBody.new_program(name, code << vars.code, line: line, prog_length: prog_length)
		end
	end # Program
	##
	#  Represents a ZX Basic program line.
	#
	#  The original program line without line number, its length and a terminating ENTER character
	#  is stored in a +body+ property as a binary string. +line_no+ is a line number.
	class Line
		attr_accessor :line_no, :body
		alias_method :line, :line_no
		def initialize(line_no, body)
			@line_no = line_no
			@body = body
		end
		##
		#  Creates a textual representation of this line except its number.
		#  Returns an UTF-8 encoded string.
		#
		#  See: Program#to_source.
		def text(escape_keywords:false, ascii_only:false)
			Tokenizer.program_data_to_text(body, escape_keywords, false, ascii_only)
		end
		##
		#  Creates a textual representation of this line with the line number.
		#  Returns an UTF-8 encoded string.
		#
		#  See: Program#to_source.
		def to_s(**opts)
			"#{line_no.to_s.rjust(4)} #{text(**opts)}"
		end

		class << self # Line
			##
			#  Creates a Basic::Line from a provided BASIC program text.
			#
			#  See: Basic.parse_source
			def parse_source_line(line_text, last_line_no=0, line_index=0)
				offset = 0
				line_no = if m = Tokenizer::Patterns::SPACES_OR_LINE_NO.match(line_text)
					line_text = m.post_match
					offset = m.end 0
					if m[1].nil?
						last_line_no.succ
					else
						m[1].to_i.tap do |n|
							raise SyntaxError, "line number must be less than or equal to 9999, in line: #{line_index}" if n > 9999
						end
					end
				else
					last_line_no.succ
				end
				if line_no <= last_line_no && !line_no.zero?
					raise SyntaxError, "line numbers must ascending, in line: #{line_index} no: #{line_no} previous: #{last_line_no}"
				end

				body = ''
				parentheses = 0
				tokenizer = Tokenizer.new line_text, line_index, offset

				body << parse_statement(tokenizer)

				while token = tokenizer.next_token
					if token.keyword?
						if token.keyword_fn?
							body << token.to_keyword_char
							if token.keyword?('BIN')
								arg = token.extract_binary_number_argument
								body << arg << ?\x0E << ZXLib::Math.pack_number(arg.gsub(Tokenizer::Patterns::SPACE_OR_CONTROL,'').to_i(2), true)
							elsif token.keyword?('THEN')
								raise SyntaxError, "unexpected THEN in line: #{line_index} at: #{token.index}" unless parentheses.zero?
								body << parse_statement(tokenizer)
							end
						else
							body << token.to_chars
						end
					elsif token.number?
						numstr = token.to_chars
						body << numstr << ?\x0E << numstr.gsub(Tokenizer::Patterns::SPACE_OR_CONTROL,'').to_f.to_z80bin
					elsif token.quote?
						body << token.to_chars << parse_string_body(tokenizer)
					elsif token.colon?
						raise SyntaxError, "unexpected colon in line: #{line_index} at: #{token.index}" unless parentheses.zero?
						body << token.to_chars << parse_statement(tokenizer)
					else
						if token.parenthesis_open?
							parentheses += 1
						elsif token.parenthesis_close?
							raise SyntaxError, "parenthesis closed too many times in line: #{line_index} at: #{token.index}" if parentheses.zero?
							parentheses -= 1
						end
						body << token.to_chars
					end
				end
				raise SyntaxError, "parentheses not closed in line: #{line_index}" unless parentheses.zero?
				Line.new line_no, body
			end

			private

			def parse_statement(tokenizer)
				buffer = ''
				while token = tokenizer.next_token
					if token.keyword_statement?
						buffer << token.to_keyword_char
						if token.keyword?('REM')
							buffer << parse_reminder_body(tokenizer)
						elsif token.keyword?('DEF FN')
							buffer << parse_def_fn_argument_list(tokenizer)
						end
						break
					elsif token.kind_of_space? || token.colon?
						buffer << token.to_chars
					else
						raise SyntaxError, "expected statement in line: #{tokenizer.line_index} at: #{token.index}"
					end
				end
				buffer
			end

			def parse_string_body(tokenizer)
				buffer = ''
				while token = tokenizer.next_token
					buffer << token.to_chars
					break if token.quote?
				end
				raise SyntaxError, "unterminated string in line: #{tokenizer.line_index}" if token.nil?
				buffer
			end

			def parse_reminder_body(tokenizer)
				buffer = ''
				while token = tokenizer.next_token
					if token.keyword_exact?
						buffer << token.to_keyword_char
					else
						buffer << token.to_chars
					end
				end
				buffer
			end

			def parse_def_fn_argument_list(tokenizer)
				buffer = ''
				tokens = tokenizer.parse_each.lazy.reject do |token|
					buffer << token.to_chars
					token.kind_of_space?
				end
				begin
					unless tokens.next.alpha_char?
						raise SyntaxError, "DEF FN name should start with an alpha character: #{tokenizer.line_index} at: #{token.index}"
					end
					token = tokens.next
					token = tokens.next if token.dollar?
					unless token.parenthesis_open?
						raise SyntaxError, "DEF FN argument list should start with a parenthesis in line: #{tokenizer.line_index} at: #{token.index}"
					end
					token = tokens.next
					unless token.parenthesis_close?
						loop do
							unless token.alpha_char?
								raise SyntaxError, "DEF FN argument should start with an alpha character: #{tokenizer.line_index} at: #{token.index}"
							end
							var_at = buffer.length
							token = tokens.next
							if token.dollar?
								var_at = buffer.length
								token = tokens.next
							end
							buffer.insert(var_at, DEF_FN_VAR_PLACEHOLDER)
							break if token.parenthesis_close?
							unless token.comma?
								raise SyntaxError, "DEF FN argument should be separated with a comma: #{tokenizer.line_index} at: #{token.index}"
							end
							token = tokens.next
						end
					end
					unless tokens.next.equals_sign?
						raise SyntaxError, "DEF FN expecting equals sign after argument list: #{tokenizer.line_index} at: #{token.index}"
					end
					raise StopIteration if tokenizer.terminated?
				rescue StopIteration
					raise SyntaxError, "DEF FN unexpected end of line: #{tokenizer.line_index}"
				end
				buffer
			end
		end # class << Line
	end # Line

	class << self # Basic
		##
		#  Creates a Basic::Program instance from a ZX Spectrum's raw binary data.
		#
		#  The binary data may be a snapshot of ZX Spectrum's memory (PROG + VARS) or taken from a TAP chunk.
		#  
		#  Provide program +data+ as a binary (ASCII-8BIT) string.
		#  Provide +prog_length+ argument to indicate the size of the program itself.
		#  Any data after +prog_length+ bytes will be interpreted as program variables.
		#
		#  Additionally +:start+ argument may be provided to indicate the starting line of a program.
		#  This information will be used when saving program as a TAP file.
		def from_program_data(data, prog_length=nil, start:nil)
			if prog_length.nil?
				prog_length = data.bytesize
			else
				raise "program size must not exceed data size" if prog_length > data.bytesize
			end
			prog, vars = data.unpack("a#{prog_length}a*")
			lines = []
			while true
				no, size, prog = prog.unpack("S>S<a*")
				break if no.nil?
				body, eol, prog = prog.unpack("a#{size - 1}Ca*")
				raise "Invalid program line" unless eol == 13
				lines << Line.new(no, body)
			end
			Program.new lines, vars, start
		end
		##
		#  Creates a Basic::Program or a Basic::Variable depending on the type of the chunk.
		#  The chunk should be a Z80::TAP::HeaderBody instance obtained from e.g. Z80::TAP.read_chunk.
		def from_tap_chunk(chunk)
			if chunk.program?
				from_program_data chunk.body.data, chunk.header.p2, start: chunk.header.p1
			elsif chunk.array?
				data = [chunk.header.array_head, chunk.header.length, chunk.body.data].pack('Cva*')
				Variable.from_data data
			else
				raise "expected a program, a character array or a number array chunk"
			end
		end
		##
		#  Creates a Basic::Program or a Basic::Variable from a TAP file.
		#
		#  See Z80::TAP.read_chunk for arguments description.
		def read_tap(filename, **opts)
			Z80::TAP.read_chunk(filename, **opts) { |chunk| from_tap_chunk(chunk) }
		end
		##
		#  Creates a Basic::Program from a BASIC program text.
		#
		#  The +source+ should be an UTF-8 encoded string.
		#
		#  Each new line separator terminates each BASIC line. The line may start with a decimal
		#  indicating the line number but doesn't have to. In this instance the last line
		#  number + 1 will be used. The line numbers must be ascending, must not be negative
		#  or larger than 9999.
		#
		#  The text is being decoded according to special character conversion rules.
		#  For details please consult Basic::Program#to_source.
		#
		#  ====Note:
		#  This method doesn't interpret the BASIC program. A lot of nonsense will be accepted.
		#  However the program text is interpreted using a Basic::Tokenizer and some simple heuristics,
		#  mainly to ensure the proper sytax of strings, numbers and some specific expressions:
		#
		#  * All numbers outside of string literals are followed by a character code 14 and 5 bytes of their
		#    internal representation in FP format (see ZXLib::Math).
		#  * After every argument of a DEF FN header list a character code 14 and 5 placeholder bytes
		#    are being added.
		#  * Literal strings are being tracked, ensuring they are properly closed.
		#  * Argument after the +BIN+ keyword will be interpreted as a binary number.
		#  * Opened and closed parentheses are counted ensuring they are properly balanced.
		#  * A statement keyword is expected (except spaces, control characters and colons) at the beginning
		#    of each line, after the colon character or after a +THEN+ keyword. In these instances only
		#    the statement keywords are being accepted.
		#  * Inside parentheses the colon character or a +THEN+ keyword is forbidden.
		#  * If a statement keyword is not expected only the keywords that may be used in expression context
		#    are converted to ZX Basic keywords.
		#  * After the +REM+ statement most of the rules are being relaxed until the end of the line.
		#
		#  Additionally +:start+ argument may be provided to indicate a starting line of a program.
		#  This information will be used when saving program as a TAP file.
		def parse_source(source, start:nil)
			unless source.force_encoding(Encoding::UTF_8).valid_encoding?
				raise "invalid program source encoding, expecting: UTF-8"
			end
			last_line_no = -1
			lines = []
			source.each_line.with_index do |line_text, line_index|
				line_text.chomp!
				line = Line.parse_source_line(line_text, last_line_no, line_index + 1)
				last_line_no = line.line_no
				lines << line
			end
			Program.new(lines, Vars.new, start)
		end
		##
		#  Creates a Basic::Program from a BASIC text file.
		#
		#  See parse_source for details.
		def read_source(filename, **opts)
			parse_source IO.read(filename, encoding: Encoding::UTF_8), **opts
		end
	end # class << Basic
	##
	#  A Basic program tokenizer.
	class Tokenizer
		def Tokenizer.program_data_to_text(data, escape_keywords=false, string_variable=false, ascii_only=false) # :nodoc:
			res = ''.force_encoding(Encoding::UTF_8)
			bytes = data.each_byte
			string_is_opened = string_variable
			escape_codes = if ascii_only then ESCAPE_CODES_ASCII_ONLY else ESCAPE_CODES end
			while true
				begin
					c = bytes.next
					if c == 34 # a quote
						if string_variable
							res << '"' 
						else
							string_is_opened = !string_is_opened
						end
					end
					case c
					when 16..21 # color control
						res << "`#{COLOR_CTRL[c - 16]} #{bytes.next}`"
					when 22 # cursor control
						res << "`AT #{bytes.next},#{bytes.next}`"
					when 23 # tab control
						res << "`TAB #{bytes.next + (bytes.next << 8)}`"
					when 0...KEYWORD_START_CODE
						if c == 14 &&  # a number
							5.times { bytes.next }
							next
						end unless string_is_opened
						if e = escape_codes[c]
							escaped = ?` + e
							begin
								while e = escape_codes[bytes.peek]
									escaped << e
									bytes.next
								end
							ensure
								res << escaped.rstrip << ?`
							end
						else
							res << CHAR_TABLE[c]
						end
					else
						keyword = KEYWORDS[c - KEYWORD_START_CODE]
						if escape_keywords || string_is_opened
							res << "`#{keyword.strip}`"
						else
							keyword = keyword.lstrip if res.empty? || /\s/ === res[-1]
							res << keyword
						end
					end
				rescue StopIteration
					return res
				end
			end
		end

		Token = ::Struct.new :index, :source, :chars, :keyword_code do
			def terminator?
				source.nil?
			end
			def to_chars
				chars
			end
			def to_keyword_char
				[keyword_code].pack('C')
			end
			def keyword?(name=nil)
				if name.nil?
					!keyword_code.nil?
				elsif keyword_code.nil?
					false
				else
					KEYWORDS[keyword_code - KEYWORD_START_CODE].strip.casecmp(name).zero?
				end
			end
			def keyword_exact?
				!!unless keyword_code.nil?
					KEYWORDS[keyword_code - KEYWORD_START_CODE] == source
				end
			end
			def keyword_fn?
				!!unless keyword_code.nil?
					keyword_code < STATEMENT_START_CODE || STATEMENTS_AS_EXPRESSIONS_CODES.include?(keyword_code)
				end
			end
			def keyword_statement?
				!!unless keyword_code.nil?
					keyword_code >= STATEMENT_START_CODE
				end
			end
			def kind_of_space?
				keyword_code.nil? && Patterns::SPACE_LIKE === chars
			end
			def extract_binary_number_argument
				if m = Patterns::BINARY_EXPR_MATCH_EXTRACT.match(chars)
					m[1]
				end
			end
			def number?
				Patterns::NUMBER_EXACT_MATCH === chars
			end
			def alpha_char?
				Patterns::ALPHA_MATCH_EXACT === chars
			end
			def colon?
				chars == ':'
			end
			def comma?
				chars == ','
			end
			def dollar?
				chars == '$'
			end
			def equals_sign?
				chars == '='
			end
			def parenthesis_open?
				chars == '('
			end
			def parenthesis_close?
				chars == ')'
			end
			def quote?
				chars == '"'
			end
		end # Token

		attr_accessor :line_index
		##
		#  Creates new instance of a Basic::Tokenizer.
		#
		#  +text+ must be an UTF-8 encoded, +line_index+ and +line_offset+ are for error messages.
		def initialize(text, line_index=0, line_offset=0)
			@source = text
			@index = line_offset.to_i
			@token = nil
			@line_index = line_index
		end
		def terminated?
			peek_token.terminator?
		end
		def parse_each(&block)
			enu = ::Enumerator.new do |y|
				while token = next_token
					y << token
				end
			end
			if block_given?
				enu.each(&block)
			else
				enu
			end
		end
		def next_token
			token = peek_token
			unless token.terminator?
				@token = nil
				token
			end
		end
		def peek_token
			if @token.nil?
				if @source.empty?
					@token = Token.new @index, nil, "", nil
				elsif m = Patterns::ESCAPE_MATCH.match(@source)
					escstr = m.to_s
					escexpr = m[1]
					offset = m.end 0
					@source = m.post_match
					if m = Patterns::COLOR_CTRL_MATCH_EXACT.match(escexpr)
						ctrl = CTRL_CODES[m[1]]
						val = m[2].to_i
						unless (0..31) === val
							raise SyntaxError, "special control arguments must be in a 0..31 range: #{escstr} in line: #{@line_index} at: #{@index}"
						end
						chars = [ctrl, val].pack('CC')
						@token = Token.new @index, chars, chars, nil
					elsif m = Patterns::CURSOR_CTRL_MATCH_EXACT.match(escexpr)
						ctrl = CTRL_CODES[m[1]]
						y, x = m[2].to_i, m[3]
						if x.nil?
							chars = [ctrl, y].pack('Cv')
						else
							chars = [ctrl, y, x.to_i].pack('CCC')
						end
						@token = Token.new @index, chars, chars, nil
					elsif m = Patterns::KEYWORDS_MATCH_EXACT.match(escexpr)
						code = KEYWORD_CODES[m.to_s]
						@token = Token.new @index, m.to_s, [code].pack('C'), code
					else
						chars = ''
						while m = Patterns::ESCAPE_TOKEN_MATCH.match(escexpr)
							tok = m[1]
							unless code = NON_ASCII_ESCAPE_TOKENS[tok]
								code = Integer(tok)
								unless (0..255) === code
									raise SyntaxError, "a code must be in a 0..255 range: #{escstr} in line: #{@line_index} at: #{@index}"
								end
							end
							chars << [code].pack('C')
							escexpr = m.post_match
						end
						unless escexpr.empty? && !chars.empty?
							raise SyntaxError, "unknown escape expression: #{escstr} in line: #{@line_index} at: #{@index}"
						end
						@token = Token.new @index, chars, chars, chars.bytesize == 1 && code >= KEYWORD_START_CODE ? code : nil
					end
					@index += offset
				elsif m = Patterns::BINARY_EXPR_MATCH.match(@source)
					key = m.to_s
					@token = Token.new @index, key, key, KEYWORD_CODES['BIN']
					@index += m.end 0
					@source = m.post_match
				elsif m = Patterns::KEYWORDS_MATCH.match(@source)
					key = m.to_s
					@token = Token.new @index, key, key, KEYWORD_CODES[key.strip]
					@index += m.end 0
					@source = m.post_match
				elsif m = Patterns::NUMBER_MATCH.match(@source)
					@token = Token.new @index, m.to_s, m.to_s, nil
					@index += m.end 0
					@source = m.post_match
				else
					src = @source.slice!(0)
					code = CHAR_CODES[src]
					if code.nil?
						raise SyntaxError, "not sure what to do with the character: \\u#{src.ord.to_s(16).rjust(4,?0)} in line: #{@line_index} at: #{@index}"
					end
					@token = Token.new @index, src, [code].pack('C'), nil
					@index += 1
				end
			end
			@token
		end
	end # Tokenizer

	ARROWS = %w[← → ↓ ↑].map{|s| s.force_encoding Encoding::UTF_8 }.freeze
	COLOR_CTRL = %w[INK PAPER FLASH BRIGHT INVERSE OVER].freeze
	CURSOR_CTRL = %w[AT TAB].freeze
	PRINTABLE_CHARS = [
		' ',
		'!',
		'"',
		'#',
		'$',
		'%',
		'&',
		"'",
		'(',
		')',
		'*',
		'+',
		',',
		'-',
		'.',
		'/',
		'0',
		'1',
		'2',
		'3',
		'4',
		'5',
		'6',
		'7',
		'8',
		'9',
		':',
		';',
		'<',
		'=',
		'>',
		'?',
		'@',
		'A',
		'B',
		'C',
		'D',
		'E',
		'F',
		'G',
		'H',
		'I',
		'J',
		'K',
		'L',
		'M',
		'N',
		'O',
		'P',
		'Q',
		'R',
		'S',
		'T',
		'U',
		'V',
		'W',
		'X',
		'Y',
		'Z',
		'[',
		'\\',
		']',
		'^',
		'_',
		'£',
		'a',
		'b',
		'c',
		'd',
		'e',
		'f',
		'g',
		'h',
		'i',
		'j',
		'k',
		'l',
		'm',
		'n',
		'o',
		'p',
		'q',
		'r',
		's',
		't',
		'u',
		'v',
		'w',
		'x',
		'y',
		'z',
		'{',
		'|',
		'}',
		'~',
		'©',
		'░',
		'▝',
		'▘',
		'▀',
		'▗',
		'▐',
		'▚',
		'▜',
		'▖',
		'▞',
		'▌',
		'▛',
		'▄',
		'▟',
		'▙',
		'█',
		'Ⓐ',
		'Ⓑ',
		'Ⓒ',
		'Ⓓ',
		'Ⓔ',
		'Ⓕ',
		'Ⓖ',
		'Ⓗ',
		'Ⓘ',
		'Ⓙ',
		'Ⓚ',
		'Ⓛ',
		'Ⓜ',
		'Ⓝ',
		'Ⓞ',
		'Ⓟ',
		'Ⓠ',
		'Ⓡ',
		'Ⓢ',
		'Ⓣ',
		'Ⓤ',
	].map{|s| s.force_encoding Encoding::UTF_8 }.freeze

	KEYWORDS = [
		'RND',
		'INKEY$',
		'PI',
		'FN ',
		'POINT ',
		'SCREEN$ ',
		'ATTR ',
		'AT ',
		'TAB ',
		'VAL$ ',
		'CODE ',
		'VAL ',
		'LEN ',
		'SIN ',
		'COS ',
		'TAN ',
		'ASN ',
		'ACS ',
		'ATN ',
		'LN ',
		'EXP ',
		'INT ',
		'SQR ',
		'SGN ',
		'ABS ',
		'PEEK ',
		'IN ',
		'USR ',
		'STR$ ',
		'CHR$ ',
		'NOT ',
		'BIN ',
		' OR ',
		' AND ',
		'<=',
		'>=',
		'<>',
		' LINE ',
		' THEN ',
		' TO ',
		' STEP ',
		' DEF FN ',
		' CAT ',
		' FORMAT ',
		' MOVE ',
		' ERASE ',
		' OPEN #',
		' CLOSE #',
		' MERGE ',
		' VERIFY ',
		' BEEP ',
		' CIRCLE ',
		' INK ',
		' PAPER ',
		' FLASH ',
		' BRIGHT ',
		' INVERSE ',
		' OVER ',
		' OUT ',
		' LPRINT ',
		' LLIST ',
		' STOP ',
		' READ ',
		' DATA ',
		' RESTORE ',
		' NEW ',
		' BORDER ',
		' CONTINUE ',
		' DIM ',
		' REM ',
		' FOR ',
		' GO TO ',
		' GO SUB ',
		' INPUT ',
		' LOAD ',
		' LIST ',
		' LET ',
		' PAUSE ',
		' NEXT ',
		' POKE ',
		' PRINT ',
		' PLOT ',
		' RUN ',
		' SAVE ',
		' RANDOMIZE ',
		' IF ',
		' CLS ',
		' DRAW ',
		' CLEAR ',
		' RETURN ',
		' COPY '
	].freeze

	NON_ASCII_ESCAPE_TOKENS = {
		'<'    => 0x08, # ←
		'>'    => 0x09, # →
		'v'    => 0x0A, # ↓
		'^'    => 0x0B, # ↑
		'&'    => 0x60, # £
		'(c)'  => 0x7F, # ©
		'|8'   => 0x80, # ░
		'|1'   => 0x81, # ▝
		'|2'   => 0x82, # ▘
		'|3'   => 0x83, # ▀
		'|4'   => 0x84, # ▗
		'|5'   => 0x85, # ▐
		'|6'   => 0x86, # ▚
		'|7'   => 0x87, # ▜
		'#7'   => 0x88, # ▖
		'#6'   => 0x89, # ▞
		'#5'   => 0x8A, # ▌
		'#4'   => 0x8B, # ▛
		'#3'   => 0x8C, # ▄
		'#2'   => 0x8D, # ▟
		'#1'   => 0x8E, # ▙
		'#8'   => 0x8F, # █
		'a'    => 0x90, # UDG A
		'b'    => 0x91, # UDG B
		'c'    => 0x92, # UDG C
		'd'    => 0x93, # UDG D
		'e'    => 0x94, # UDG E
		'f'    => 0x95, # UDG F
		'g'    => 0x96, # UDG G
		'h'    => 0x97, # UDG H
		'i'    => 0x98, # UDG I
		'j'    => 0x99, # UDG J
		'k'    => 0x9A, # UDG K
		'l'    => 0x9B, # UDG L
		'm'    => 0x9C, # UDG M
		'n'    => 0x9D, # UDG N
		'o'    => 0x9E, # UDG O
		'p'    => 0x9F, # UDG P
		'q'    => 0xA0, # UDG Q
		'r'    => 0xA1, # UDG R
		's'    => 0xA2, # UDG S
		't'    => 0xA3, # UDG T
		'u'    => 0xA4, # UDG U
	}

	KEYWORD_START_CODE = 0xA5
	STATEMENT_START_CODE = 0xCE

	CHAR_TABLE = Array.new(32, "") + PRINTABLE_CHARS
	CHAR_TABLE[6] = "\t"
	ARROWS.each_with_index {|arrow, i| CHAR_TABLE[i + 8] = arrow }
	# CHAR_TABLE[14] = "?"
	(16..23).each {|i| CHAR_TABLE[i] = "?" }
	ESCAPE_CODES = Hash[CHAR_TABLE.each_with_index.select{|s,| s.empty?}.map { |_,i| [i, "#{i} "]  }]
	ESCAPE_CODES_ASCII_ONLY = ESCAPE_CODES.dup
	NON_ASCII_ESCAPE_TOKENS.each { |s, c| ESCAPE_CODES_ASCII_ONLY[c] = s }
	raise SyntaxError unless CHAR_TABLE.length == KEYWORD_START_CODE
	CHAR_TABLE.each{|s| s.force_encoding Encoding::UTF_8 }
	CHAR_TABLE.freeze
	ESCAPE_CODES.freeze
	ESCAPE_CODES_ASCII_ONLY.freeze

	STATEMENTS_AS_EXPRESSIONS_CODES = [*(0xD9..0xDE), 0xE4] # color expressions and DATA (in LOAD "" DATA)
	DEF_FN_VAR_PLACEHOLDER = ?\x0E + ?\x00*5
	KEYWORD_CODES = Hash[KEYWORDS.map.with_index {|key, index| [key.strip, index + KEYWORD_START_CODE] }]
	CTRL_CODES = Hash[(COLOR_CTRL + CURSOR_CTRL).map.with_index {|key, index| [key, index + 0x10] }]
	CHAR_CODES = Hash[
		[[?\t, 6]] +
		ARROWS.map.with_index {|a,i| [a, i + 8]} +
		PRINTABLE_CHARS.map.with_index {|a,i| [a, i + 0x20]}
	]

	class Tokenizer
		module Patterns
			SPACES_OR_LINE_NO = /\A\s*(?:(\d+)\s?)?/
			SPACE_OR_CONTROL = /[\x00-\x20]/
			SPACE_LIKE = /\A[\x00-\x20]/
			NUMBER_MATCH = /\A(?:\d*\.(?:[\x00-\x20]*\d)+(?:[\x00-\x20]*e[\x00-\x20]*[-+]?[\x00-\x20]*\d+)?|(?:\d+e[\x00-\x20]*[-+]?[\x00-\x20]*\d+)|\d+)/i
			NUMBER_EXACT_MATCH = /#{NUMBER_MATCH}\z/
			BINARY_EXPR_MATCH = /\ABIN(?:[\x00-\x20]*[01])*/
			BINARY_EXPR_MATCH_EXTRACT = /\ABIN((?:[\x00-\x20]*[01])*)\z/
			ESCAPE_MATCH = /\A`([^`]*)(?:`|\z)/
			VARNAME_MATCH_EXACT = /\A[[:alpha:]][[:alnum:]]*\z/
			VARSTRNAME_MATCH_EXACT = /\A[[:alpha:]]\$\z/
			ALPHA_MATCH_EXACT = /\A[[:alpha:]]\z/
			KEYWORDS_MATCH = Regexp.union(Basic::KEYWORDS.sort { |x,y| y.length <=> x.length }.map { |stmnt|
				if stmnt.start_with? ' '
					if stmnt.end_with? ' '
						/\A\s?#{Regexp.quote stmnt.strip}\b\s?/
					else
						/\A\s?#{Regexp.quote stmnt.lstrip}/
					end
				else
					if stmnt.end_with? ' '
						stmnt = stmnt.rstrip
						if stmnt.end_with? '$'
							/\A#{Regexp.quote stmnt}\B\s?/
						else
							/\A#{Regexp.quote stmnt}\b\s?/
						end
					else
						/\A#{Regexp.quote stmnt}/
					end					
				end
			})
			KEYWORDS_MATCH_EXACT = /\A#{Regexp.union(Basic::KEYWORDS.map(&:strip).sort { |x,y| y.length <=> x.length })}\z/
			NON_ASCII_ESCAPE_TOKEN_MATCH = Regexp.union(NON_ASCII_ESCAPE_TOKENS.keys)
			ESCAPE_TOKEN_MATCH = /\A\s*(0x[0-9a-fA-F]+|0b[01]+|0[0-7]+|0|[1-9]\d*|#{NON_ASCII_ESCAPE_TOKEN_MATCH})\s*,?/
			COLOR_CTRL_MATCH_EXACT = /\A(#{Regexp.union(Basic::COLOR_CTRL)})\s*(\d+)\z/
			CURSOR_CTRL_MATCH_EXACT = /\A(#{Regexp.union(Basic::CURSOR_CTRL)})\s*(\d+)\s*(?:,\s*(\d+)\s*)?\z/
		end
	end

	#---------------------------------- Vars ---------------------------------#
	module VariableTypes
		VAR_STRING       = 0b010
		VAR_NUMBER       = 0b011
		VAR_NUMBER_ARRAY = 0b100
		VAR_NUMBER_EX    = 0b101
		VAR_CHAR_ARRAY   = 0b110
		VAR_FOR_LOOP     = 0b111
	end
	##
	#  A container class for keeping and inspecting ZX Basic program variables.
	#
	#  Example:
	#
	#    require 'date'
	#    require 'zxlib/basic'
	#    
	#    program = Basic.parse_source <<-END
	#       1 GO SUB 1000
	#         LOAD ""DATA m$()
	#         GO SUB 1000
	#     999 STOP
	#    1000 INPUT "month number: ", mn
	#         IF INT mn>=1 AND INT mn<=12 THEN PRINT m$(INT mn): GO TO 1000
	#         RETURN
	#    END
	#    program.start = 1
	#    program.vars << Basic::Variable.new_char_array('m$', [12, 3], Date::ABBR_MONTHNAMES[1..12])
	#    program.save_tap 'askmonth.tap'
	#    
	#    monthnames = Date::MONTHNAMES[1..12]
	#    maxlen = monthnames.max_by(&:length).length
	#    mnames_var = Basic::Variable.new_char_array('m$', [12, maxlen], monthnames)
	#    mnames_var.save_tap 'askmonth.tap', append: true, name: 'longmonths'
	#    
	class Vars
		attr_accessor :data
		alias_method :code, :data
		##
		#  Creates an instance of Basic::Vars
		#
		#  Optionally provide VARS data as a binary string.
		def initialize(data='')
			data = data.data if data.is_a?(self.class)
			raise ArgumentError, "data must be a binary string" unless String === data
			@data = data.force_encoding(Encoding::BINARY)
		end
		##
		#  Clear all variables
		def clear!
			@data = ''
		end
		##
		#  Adds a Basic::Variable to self.
		def <<(var)
			raise ArgumentError unless var.respond_to?(:data)
			self.data << var.data
		end
		##
		#  Returns an Enumerator of every Basic::Variable found in self.
		def each_var(&block)
			data = @data
			enu = ::Enumerator.new do |y|
				while !data.empty?
					v = Variable.from_data data
					data = data.byteslice(v.bytesize..-1)
					y << v
				end
			end
			if block_given?
				enu.each(&block)
			else
				enu
			end
		end
		##
		#  Returns an array of every Basic::Variable found in self.
		def to_a
			each_var.to_a
		end
		##
		#  Returns a first Basic::Variable if found by its name.
		def get(name)
			each_var.find{|v| v.name.casecmp(name).zero? }
		end
		##
		#  Returns a Basic::Variable at +index+ or an array of variables if +Range+ is given.
		def [](index)
			if Integer === index
				each_var.with_index {|v, i| break v if index == i}
			elsif index.respond_to?(:===)
				each_var.with_index.select {|v, i| index === i}
			end
		end
		##
		#  Returns all variables in a BASIC-like text format.
		def to_s
			each_var.map(&:to_s).join("\n")
		end
		##
		#  Converts a ZX-Basic's string variable body to a UTF-8 Basic program text.
		def Vars.string_to_program_text(data, ascii_only:false)
			Tokenizer.program_data_to_text(data, true, true, ascii_only)
		end
	end # Vars
	##
	#  Converts a UTF-8 Basic program string to a ZX-Basic string variable body.
	def Vars.program_text_to_string(text)
		buffer = ''
		tokenizer = Tokenizer.new text
		while token = tokenizer.next_token
			buffer << token.to_chars
			if token.quote?
				token = tokenizer.next_token
				break if token.nil?
				unless token.quote?
					raise SyntaxError, "quote must be duplicated in a string at: #{token.index}"
				end
			end
		end
		buffer
	end
	##
	#  Represents a ZX Spectrum's Basic variable with various methods to inspect its content.
	class Variable
  	include VariableTypes
		include Z80::TAP
		##
		#  Creates a Z80::TAP::HeaderBody instance from Basic::Variable.
		#
		#  This method is provided for the included Z80::TAP#to_tap and Z80::TAP#save_tap methods.
		def to_tap_chunk(name, org:nil)
			if array?
				Z80::TAP::HeaderBody.new_var_array(name, code, head)
			else
				Z80::TAP::HeaderBody.new_code(name, code, org || 0x5B00)
			end
		end
		##
		#  Returns a header byte.
		def head
			data.ord
		end
		##
		#  Returns a portion of data after the header.
		def code
			case type
			when VAR_NUMBER, VAR_NUMBER_EX
				data.byteslice(-5..-1)
			when VAR_FOR_LOOP
				data.byteslice(1, 18)
			else
				data.byteslice(3..-1)
			end
		end

		## The type of a variable; one of VariableTypes.
		attr_reader :type
		## The original name of a variable.
		attr_reader :name
		## The variable data in its original format.
		attr_reader :data
		## The dimension sizes as an array of integers. Only for array variables.
		attr_reader :dims

		def initialize(type, name, data, dims=nil)
			@type = type
			@name = name
			@data = data
			@dims = dims
		end
		## +true+ if variable is a string variable
		def string?
			type == VAR_STRING
		end
		## +true+ if variable is a number variable
		def number?
			type == VAR_NUMBER || type == VAR_NUMBER_EX
		end
		## +true+ if variable is a number or character array
		def array?
			!dims.nil?
		end
		## +true+ if variable is a character array
		def char_array?
			type == VAR_CHAR_ARRAY
		end
		## +true+ if variable is a number array
		def number_array?
			type == VAR_NUMBER_ARRAY
		end
		## +true+ if variable is a FOR loop variable
		def for_loop?
			type == VAR_FOR_LOOP
		end
		##
		#  For strings returns the original string length, for arrays a number of dimensions.
		def length
			case type
			when VAR_STRING
				bytesize - 3
			when VAR_NUMBER_ARRAY, VAR_CHAR_ARRAY
				dims.length
			end
		end
		##
		#  Returns original size of this variable in bytes.
		def bytesize
			data.bytesize
		end
		##
		#  Returns a value of a variable.
		#
		#  * A Float or an Integer for numbers (including FOR loops).
		#  * A (possibly nested) array of values for array variables.
		#  * A string, suitable to be inserted as a program literal, for string variables.
		def value
			case type
			when VAR_STRING
				Vars.string_to_program_text code
			when VAR_NUMBER, VAR_NUMBER_EX, VAR_FOR_LOOP
				ZXLib::Math.unpack_number code
			else
				self.[]()
			end
		end
		##
		#  Returns the FOR loop limit value.
		def limit
			if for_loop?
				ZXLib::Math.unpack_number data.byteslice(6, 5)
			end
		end
		##
		#  Returns the FOR loop step value.
		def step
			if for_loop?
				ZXLib::Math.unpack_number data.byteslice(11, 5)
			end
		end
		##
		#  Returns the FOR loop line number.
		def line
			if for_loop?
				data.unpack('@16v')[0]
			end
		end
		##
		#  Returns the FOR loop execute statement number.
		def statement
			if for_loop?
				data.unpack('@18C')[0]
			end
		end
		##
		#  Returns a selected portion of an array variable according to the provided dimension indices.
		#
		#  The indices start from 1 (not 0). Indices may be negative to indicate counting from the end.
		#  The number of indices should be equal or less than the number of dimensions.
		#  The last dimension index may be provided as a Range.
		def [](*at)
			if dims && at.length < dims.length
				last_at = 1..dims[-1]
				fun = ->(*args) { self.[](*args, last_at) }
				return dims[(at.length)...-1].reverse_each.inject(fun) { |fun, n|
					->(*args) do
						1.upto(n).map do |i|
							fun.call(*args, i)
						end
					end
				}.call(*at)
			end
			data = byteslice(*at)
			case type
			when VAR_NUMBER_ARRAY
				if Range === at.last
					0.step(data.bytesize - 1, 5).map do |offs|
						ZXLib::Math.unpack_number data.byteslice(offs, 5), false
					end
				else
					ZXLib::Math.unpack_number data
				end
			when VAR_CHAR_ARRAY, VAR_STRING
				Vars.string_to_program_text data
			else
				raise "Variable is a scalar"
			end
		end
		##
		#  Returns a selected portion of an array variable according to provided dimension indices as raw bytes.
		#
		#  The indices start from 1 (not 0). Indices may be negative to indicate counting from the end.
		#  All dimension indices must be provided. The last dimension index may be passed as a Range.
		def byteslice(*at)
			if array?
				raise "Subscript wrong" if at.length != dims.length
				start = 4 + at.length * 2
				offset = 0
				size = 1
				last_at = at.last
				if Range === last_at
					dlen = dims[-1]
					last_at = Range.new( *[last_at.begin, last_at.end].map { |x| x < 0 ? x + dlen + 1 : x },
															 last_at.exclude_end? )
					size = last_at.size
					raise "Subscript wrong" unless (1..dlen) === last_at.begin + size - 1
					at[-1] = last_at.begin
				end
				at.zip(dims).reverse_each.inject(1) do |dsiz, (x, dlen)|
					x += dlen + 1 if x < 0
					raise "Subscript wrong" unless (1..dlen) === x
					offset += (x - 1) * dsiz
					dsiz * dlen
				end
				if type == VAR_NUMBER_ARRAY
					offset *= 5 
					data.byteslice(start + offset, 5*size)
				else
					data.byteslice(start + offset, size)
				end
			elsif string?
				return code if at.empty?
				raise "Subscript wrong" if at.length != 1
				offs, = at
				len = length
				normalize = ->(x) do
					if x < 0
						x + len
					else
						x - 1
					end
				end
				if Range === offs
					offsets = [offs.begin, offs.end].map(&normalize)
					raise "Subscript wrong" unless offsets.all? { |x| (0...len) === x }
					offs = Range.new *offsets, offs.exclude_end?
				else
					offs = normalize.call(offs)
					raise "Subscript wrong" unless (0...len) === offs
				end
				code.byteslice(offs)
			end
		end
		##
		#  Returns this variable in a BASIC-like text format.
		def to_s
			case type
			when VAR_STRING
				"LET #{name}=\"#{value}\""
			when VAR_STRING, VAR_NUMBER, VAR_NUMBER_EX
				"LET #{name}=#{value}"
			when VAR_NUMBER_ARRAY, VAR_CHAR_ARRAY
				"DIM #{name}(#{dims.join ','})"
			when VAR_FOR_LOOP
				"FOR #{name}=#{value} TO #{limit} STEP #{step}, #{line}:#{statement}"
			else
				raise "unknown variable type"
			end
		end
	end # Variable

	class VariableParseError < RuntimeError
		def initialize(msg="Not a variable")
			super
		end
	end

	class << Variable
		include VariableTypes
		##
		#  Creates a numeric Basic::Variable.
		def new_number(name, num, simplified_int=true)
			raise TypeError unless String === name
			name.downcase!
			unless Tokenizer::Patterns::VARNAME_MATCH_EXACT === name
				raise ArgumentError, "name must be composed of a single alphabetic character followed by alphabetic or numeric ones"
			end
			value = ZXLib::Math.pack_number num, simplified_int
			if name.length == 1
				head = (VAR_NUMBER << 5)|(name.ord & 0b00011111)
				Variable.new VAR_NUMBER, name, [head, value].pack('Ca5')
			else
				head = ((VAR_NUMBER_EX << 5)|(name.ord & 0b00011111)).chr
				name[1...-1].each_byte do |code|
					head << (code & 0b01111111).chr
				end
				head << (name[-1].ord | 0b10000000).chr
				Variable.new VAR_NUMBER_EX, name, [head, value].pack('a*a5')
			end
		end
		##
		#  Creates a string Basic::Variable.
		#
		#  The +string+ is parsed by Vars.program_text_to_string only if encoded as +UTF-8+.
		def new_string(name, string)
			raise TypeError unless String === string && String === name
			name.downcase!
			unless Tokenizer::Patterns::VARSTRNAME_MATCH_EXACT === name
				raise ArgumentError, "name must be a single alphabetic character followed by a $ sign"
			end
			string = Vars.program_text_to_string(string) if string.encoding.name == "UTF-8".freeze
			head = (VAR_STRING << 5)|(name.ord & 0b00011111)
			Variable.new VAR_STRING, name, [head, string.bytesize, string].pack('Cva*')
		end
		##
		#  Creates a FOR loop Basic::Variable.
		def new_for_loop(name, value, limit, step, line, statement)
			raise TypeError unless String === name
			name.downcase!
			unless Tokenizer::Patterns::ALPHA_MATCH_EXACT === name
				raise ArgumentError, "name must be a single alphabetic character"
			end
			value, limit, step = [value, limit, step].map { |num| ZXLib::Math.pack_number num }
			head = (VAR_FOR_LOOP << 5)|(name.ord & 0b00011111)
			Variable.new VAR_FOR_LOOP, name, [head, value, limit, step, line, statement].pack('Ca5a5a5vC')
		end

		##
		#  Creates a numeric array Basic::Variable.
		#
		#  +dims+ must be an array of dimension sizes provided as positive integers.
		#  +values+ if provided should be a nested array of numbers of exact same dimensions as +dims+.
		def new_number_array(name, dims, values=nil)
			raise TypeError unless String === name
			name.downcase!
			unless Tokenizer::Patterns::ALPHA_MATCH_EXACT === name
				raise ArgumentError, "name must be a single alphabetic character"
			end
			raise ArgumentError, "`dims` must be an array of dimension sizes" unless dims.is_a?(Array)
			raise ArgumentError, "`dims` must not be empty" if dims.empty?
			raise ArgumentError, "number of dimensions must be less than or equal to 255" if dims.length > 255
			unless dims.all? { |v| (1..65535) === v }
				raise ArgumentError, "dimension size must be at least 1"
			end
			head = (VAR_NUMBER_ARRAY << 5)|(name.ord & 0b00011111)
			ndims = dims.length
			vsize = dims.inject(:*)
			bsize = vsize * 5 + ndims * 2 + 1
			vpacked = if values.nil?
				?\0 * (vsize * 5)
			else
				enumerate_deep_values(dims, values).inject('') do |buf, num|
					buf << ZXLib::Math.pack_number(num)
				end
			end
			Variable.new VAR_NUMBER_ARRAY, name, [head, bsize, ndims, *dims, vpacked].pack("CvCv#{ndims}a*"), dims.freeze
		end
		##
		#  Creates a character array Basic::Variable.
		#
		#  The +strings+ are parsed by Vars.program_text_to_string only if encoded as +UTF-8+.
		#
		#  +dims+ must be an array of dimension sizes provided as positive integers.
		#  +values+ if provided should be a nested array of strings of exact same dimensions as +dims+.
		def new_char_array(name, dims, values=nil)
			raise TypeError unless String === name
			name.downcase!
			unless Tokenizer::Patterns::VARSTRNAME_MATCH_EXACT === name
				raise ArgumentError, "name must be a single alphabetic character followed by a $ sign"
			end
			raise ArgumentError, "`dims` must be an array of dimension sizes" unless dims.is_a?(Array)
			raise ArgumentError, "`dims` must not be empty" if dims.empty?
			raise ArgumentError, "number of dimensions must be less than or equal to 255" if dims.length > 255
			unless dims.all? { |v| (1..65535) === v }
				raise ArgumentError, "dimension size must be at least 1"
			end
			head = (VAR_CHAR_ARRAY << 5)|(name.ord & 0b00011111)
			ndims = dims.length
			vsize = dims.inject(:*)
			bsize = vsize + ndims * 2 + 1
			vpacked = if values.nil?
				?\x20 * vsize
			else
				strsize = dims[-1]
				enumerate_deep_values(dims[0...-1], values).inject('') do |buf, str|
					str = Vars.program_text_to_string(str) if str.encoding.name == "UTF-8".freeze
					buf << str.byteslice(0, strsize).ljust(strsize)
				end
			end
			Variable.new VAR_CHAR_ARRAY, name, [head, bsize, ndims, *dims, vpacked].pack("CvCv#{ndims}a*"), dims.freeze
		end

		def enumerate_deep_values(dims, values) # :nodoc:
			::Enumerator.new do |y|
				feeder = ->(v) { y << v }
				begin
					dims.reverse_each.inject(feeder) { |fun, n|
						->(vals) do
							enu = vals.each
							n.times { fun.call enu.next }
						end
					}.call(values)
				rescue StopIteration
					raise ArgumentError, "not enough values provided"
				end
			end
		end
		##
		#  Creates a Basic::Variable from a ZX Spectrum's VARS raw data.
		#
		#  Provide +data+ as a binary string.
		def from_data(data)
			raise ArgumentError unless String === data
			head, rest = data.unpack('Ca*')
			raise VariableParseError if head.nil? || rest.nil? || rest.empty?
			name = ((head & 0b00011111) | 0b01100000).chr
			type = head >> 5
			case type
			when VAR_STRING
				len, = data.unpack('xv')
				raise VariableParseError if len.nil? || data.bytesize < len + 3
				Variable.new type, name << '$', data.byteslice(0, len + 3)
			when VAR_NUMBER
				raise VariableParseError if data.bytesize < 6
				Variable.new type, name, data.byteslice(0, 6)
			when VAR_NUMBER_EX
				data.byteslice(1..-1).each_byte do |c|
					name << (c & 0b01111111).chr
					break unless (c & 0b10000000).zero?
				end
				bsize = name.length + 5
				raise VariableParseError if data.bytesize < bsize
				Variable.new type, name, data.byteslice(0, bsize)
			when VAR_NUMBER_ARRAY, VAR_CHAR_ARRAY
				bsize, ndims, rest = data.unpack('xvCa*')
				raise VariableParseError if bsize.nil? || ndims.nil? || rest.nil? || ndims.zero? || bsize <= ndims * 7 + 1
				raise VariableParseError, "not enough array data" if rest.bytesize + 1 < bsize
				dims = rest.unpack("v#{ndims}")
				raise VariableParseError, "invalid array dimensions" if dims.any?(&:zero?)
				datasize = dims.inject(:*)
				if type == VAR_NUMBER_ARRAY
					datasize *= 5
				else
					name << '$'
				end
				raise VariableParseError, "invalid array data" if bsize != datasize + ndims * 2 + 1
				Variable.new type, name, data.byteslice(0, bsize + 3), dims.freeze
			when VAR_FOR_LOOP
				raise VariableParseError if data.bytesize < 19
				Variable.new type, name, data.byteslice(0, 19)
			else
				raise VariableParseError
			end
		end
	end # Variable
end
