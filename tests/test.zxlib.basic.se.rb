# -*- coding: BINARY -*-
require 'test/unit/assertions'
require 'z80'
require 'zxlib/basic'

include Test::Unit::Assertions
include ZXLib

def compare_programs(expected, checked)
  puts '-----------------------'
  for (expline, chkline) in expected.lines.zip(checked.lines)
    puts expline.to_s se:true
    puts chkline.to_s se:false
    assert_equal expline.line, chkline.line
    assert_equal expline.body.bytes, chkline.body.bytes
    puts
  end
  assert_equal expected.code, checked.code
end


chunk = Z80::TAP.read_chunk File.expand_path('test.zxlib.basic.se.tap', __dir__)
program = Basic.from_tap_chunk chunk
source = program.to_source
program_check = Basic.parse_source source

se_source = program.to_source se:true
program_se_check = Basic.parse_source se_source

compare_programs program, program_se_check
compare_programs program, program_check


program_check = Basic.parse_source <<-EOB
  10 COPY 0
  20 `0`0,1
  30 CAT 7
  40 `1`10
  50 ERASE
  60 FORMAT 0
  70 MOVE 0,0,0
  80 `5` GO TO 1000
  90 `5` CONTINUE
 100 `5` STOP
 110 `3`64,0
 120 `2`10,10
 130 `4`6,15;7,7
 140 INK 9
 200 PRINT &FFFF
 210 PRINT \\177,-\\177
 220 PRINT ~&ffff
 230 PRINT &ff,-&ff
EOB
compare_programs program, program_check


program_se_check = Basic.parse_source <<-EOB
  10 CALL 0
  20 DELETE 0,1
  30 DIR 7
  40 EDIT 10
  50 ERASE
  60 FORMAT 0
  70 MOVE 0,0,0
  80 ON ERROR GO TO 1000
  90 ON ERROR CONTINUE
 100 ON ERROR STOP
 110 PALETTE 64,0
 120 RENUM 10,10
 130 SOUND 6,15;7,7
 140 PEN 9
 200 PRINT &FFFF
 210 PRINT \\177,-\\177
 220 PRINT ~&ffff
 230 PRINT &ff,-&ff
EOB
compare_programs program, program_se_check
