# -*- coding: BINARY -*-
require('z80')
##
#  ==A module with BASIC program data parser
#
#  Example:
#
#    require('zxlib/basic')
#    chunk = Z80::TAP.read_chunk('examples/calculator.tap')
#    puts Basic.parse_program(chunk.body.data).to_s
#
module Basic
	class << self
		Program = ::Struct.new :lines, :vars do
			def to_s
				lines.map(&:to_s).join("\n")
			end
		end

		Line = ::Struct.new :line, :text do
			def to_s
				"#{line.to_s.rjust(4)} #{text}"
			end
		end

		def parse_program(program, prg_size=nil)
			prg_size = program.bytesize if prg_size.nil?
			program, = program.unpack("a#{prg_size}")
			lines = []
			while true
				no, size, program = program.unpack("S>S<a*")
				break if no.nil?
				text, eol, program = program.unpack("a#{size - 1}Ca*")
				raise "Invalid program line" unless eol == 13
				lines << Line.new(no, Basic.text_to_s(text))
			end
			Program.new(lines, nil)
		end

		def text_to_s(text)
			res = ''
			bytes = text.each_byte
			while true
				begin
					c = bytes.next
					case c
					when 8..11
						res << ARROWS[c - 8]
					when 0x0E # a number
						5.times { bytes.next }
					when 32..164
						res << CHAR_TABLE[c-32]
					when 165..255
						res << " " if !res.empty? && /[a-z0-9":]/i === res[-1]
						res << CHAR_TABLE[c-32] + " "
					else
						res << '?'
					end
				rescue StopIteration
					return res
				end
			end
		end
	end

	ARROWS = ["←","→","↓","↑"]

	CHAR_TABLE = [
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
		'/',
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
		'-',
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
		'RND',
		'INKEY$',
		'PI',
		'FN',
		'POINT',
		'SCREEN$',
		'ATTR',
		'AT',
		'TAB',
		'VAL$',
		'CODE',
		'VAL',
		'LEN',
		'SIN',
		'COS',
		'TAN',
		'ASN',
		'ACS',
		'ATN',
		'LN',
		'EXP',
		'INT',
		'SOR',
		'SGN',
		'ABS',
		'PEEK',
		'IN',
		'USR',
		'STR$',
		'CHR$',
		'NOT',
		'BIN',
		'OR',
		'AND',
		'<=',
		'>=',
		'<>',
		'LINE',
		'THEN',
		'TO',
		'STEP',
		'DEF FN',
		'CAT',
		'FORMAT',
		'MOVE',
		'ERASE',
		'OPEN #',
		'CLOSE #',
		'MERGE',
		'VERIFY',
		'BEEP',
		'CIRCLE',
		'INK',
		'PAPER',
		'FLASH',
		'BRIGHT',
		'INVERSE',
		'OVER',
		'OUT',
		'LPRINT',
		'LLIST',
		'STOP',
		'READ',
		'DATA',
		'RESTORE',
		'NEW',
		'BORDER',
		'CONTINUE',
		'DIM',
		'REM',
		'FOR',
		'GO TO',
		'GO SUB',
		'INPUT',
		'LOAD',
		'LIST',
		'LET',
		'PAUSE',
		'NEXT',
		'POKE',
		'PRINT',
		'PLOT',
		'RUN',
		'SAVE',
		'RANDOMIZE',
		'IF',
		'CLS',
		'DRAW',
		'CLEAR',
		'RETURN',
		'COPY',
	]
end
