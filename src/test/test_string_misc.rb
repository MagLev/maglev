# Misellaneous tests for string (e.g., regressions and random corner cases)

require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

#

test 'fooo'.rindex('oo'),    2,   "GemStone rindex A"
test 'foooo'.rindex('oo'),   3,   "GemStone rindex B" # throws ST error...
test 'abcabc'.rindex('abc'), 3,   "GemStone rindex C"
test 'abcabc'.rindex('xyz'), nil, "GemStone rindex D"
test ''.rindex('abc'),       nil, "GemStone rindex E"
test ''.rindex(''),          0,   "GemStone rindex F"
test 'abc'.rindex(''),       3,   "GemStone rindex G"
test 'ruby.rbx'.rindex('x'), 7,   "GemStone rindex H"  # Regression
test 'rubx.rbx'.rindex('x'), 7,   "GemStone rindex I"  # Regression

s = "-rw-r--r--  1 650  0  0 Oct 20  1999 /tmp/FileStatTest-234\n"
test s.split, ["-rw-r--r--", "1", "650", "0", "0", "Oct", "20", "1999", "/tmp/FileStatTest-234"], "GemStone split A"

test(" now's   the time".split,      ["now's", "the", "time"],  "PA split A")
test(" now's   the time".split(' '), ["now's", "the", "time"],  "PA split B")
test(" now's   the time".split(/ /), ["", "now's", "", "", "the", "time"], "PA split C")

test("a@1bb@2ccc".split(/@\d/),     ["a", "bb", "ccc"],           "PA split D")
test("a@1bb@2ccc".split(/@(\d)/),   ["a", "1", "bb", "2", "ccc"], "PA split E")
test("1, 2.34,56, 7".split(/,\s*/), ["1", "2.34", "56", "7"],     "PA split F")

test("hello".split(//),      ["h", "e", "l", "l", "o"], "PA split G")
test("hello".split(//, 3),   ["h", "e", "llo"],         "PA split H")
test("hi mom".split(/\s*/),  ["h", "i", "m", "o", "m"], "PA split I")
test("".split,               [],                        "PA split J")

test "mellow yellow".split("ello"), ["m", "w y", "w"], "PA split J"

test "1,2,,3,4,,".split(','),     ["1", "2", "", "3", "4"], "PA split K"
test "1,2,,3,4,,".split(',',4),   ["1", "2", "", "3,4,,"],  "PA split L"
#test "1,2,,3,4,,".split(',', -4), ["1", "2", "", "3", "4", "", ""], "PA split M"

# Test that string.inspect uses double quotes etc.
test("".inspect,       "\"\"",          'inspect A')
test("A".inspect,      "\"A\"",         'inspect B')
test("\A".inspect,     "\"A\"",         'inspect C')
test("\\A".inspect,    "\"\\\\A\"",     'inspect D')
test("\\\"A".inspect,  "\"\\\\\\\"A\"", 'inspect E')

foo = 1
test("#{foo}".inspect, "\"1\"",         'inspect F')
test("\t\n".inspect,   "\"\\t\\n\"",    'inspect G')


# Test that string.inspect uses double quotes etc.
test("".inspect,       "\"\"",          'inspect A')
test("A".inspect,      "\"A\"",         'inspect B')
test("\A".inspect,     "\"A\"",         'inspect C')
test("\\A".inspect,    "\"\\\\A\"",     'inspect D')
test("\\\"A".inspect,  "\"\\\\\\\"A\"", 'inspect E')

foo = 1
test("#{foo}".inspect, "\"1\"",         'inspect F')
test("\t\n".inspect,   "\"\\t\\n\"",    'inspect G')


# Integer conversions from PickAxe p 622
test('12345'.to_i,           12345, '12345')
test('99 red balloons'.to_i,    99, '99 red balloons')

test('0a'.to_i,                  0, '0a')
test('0a'.to_i(16),             10, '"0a".to_i(16)')

test('0x10'.to_i,                0, '"0x10".to_i')
test('0x10'.to_i(0),            16, '"0x10".to_i(0)')
test('-0x10'.to_i(0),          -16, '-0x10.to_i(0)')

test('hello'.to_i,                0, '"hello".to_i')
test('hello'.to_i(30),   14_167_554, '"hello".to_i(30)')

test('1100101'.to_i(2),         101, '"1100101".to_i(2)')
test('1100101'.to_i(8),     294_977, '"1100101".to_i(8)')
test('1100101'.to_i(10),  1_100_101, '"1100101".to_i(10)')
test('1100101'.to_i(16), 17_826_049, '"1100101".to_i(16)')
test('1100101'.to_i(24),  199066177, '"1100101".to_i(24)')

test("10".hex, 16, '"10".hex')
test("0x10".hex, 16, '"0x10".hex')
test("0X10".hex, 16, '"0X10".hex')
test("0X10 blue balloons".hex, 16, '"0X10 blue balloons".hex')

# From pickaxe
test('0x0a'.hex,     10, '"0x0a".hex')
test('-1234'.hex, -4660, '"-1234".hex')
test('0'.hex,         0, '"0".hex')
test('wombat'.hex,    0, '"wombat".hex')

test('123'.oct,      83, '"123".oct')
test('-377'.oct,   -255, '"-377".oct')
test('0377bad'.oct, 255, '"0377bad".oct')

# Some weird ones...
test('0b1010'.oct, 10, '"0b1010".oct')
test('0755_333'.oct, 252635, '"0755_333".oct')

# Some of these were raising exceptions at one point...
test('0a'.to_i,     0, '"0a".to_i')     # radix is 10
test('0a'.to_i(10), 0, '"0a".to_i(10)') # radix is 10
test('0a'.to_i(0),  0, '"0a".to_i(0)')

# Ensure succ is present
test('zzz'.succ, 'aaaa', '"zzz".succ')

def testChomp
  s = 'abcd'
  r = s.chomp
  unless r == s ; raise 'Err'; end
  if r.equal?(s) ; raise 'Err'; end
  r = s.chomp!
  unless r.equal?(nil) ; raise 'Err'; end

  s = "abcd\n"
  r = s.chomp
  if r.equal?(s) ; raise 'Err'; end
  unless r == 'abcd' ; raise 'Err'; end
  r = s.chomp!
  unless r.equal?(s) ; raise 'Err'; end
  unless r == 'abcd' ; raise 'Err'; end
  s = "abcd\r\n"
  r = s.chomp
  if r.equal?(s) ; raise 'Err'; end
  unless r == 'abcd' ; raise 'Err'; end
  r = s.chomp!
  unless r.equal?(s) ; raise 'Err'; end
  unless r == 'abcd' ; raise 'Err'; end

  s = 'abcd'
  r = s.chop
  unless r == 'abc' ; raise 'Err'; end
  if r.equal?(s) ; raise 'Err'; end
  r = s.chop!
  unless r == 'abc' ; raise 'Err'; end
  unless r.equal?(s) ; raise 'Err'; end
  s = '' 
  r = s.chop! 
  unless r.equal?(nil) ; raise 'Err'; end 
  r = s.chop 
  unless r.length == 0 ; raise 'Err'; end
  return true
end

test(self.testChomp() , true, "testing chomp, chop")
  
# gsub was generating undefined method for each_match.  This tests that case:
test('a.rb'.gsub('\\', ''), 'a.rb',  'gsub regression')

report

