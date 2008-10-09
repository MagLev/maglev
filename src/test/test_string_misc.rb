# Misellaneous tests for string (e.g., regressions and random corner cases)

$failed = []
$count = 0
def test(actual, expected, msg)
    $count += 1
    $failed << "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}" unless expected == actual
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  puts $failed
  raise $failed.join("\n") unless $failed.empty?
end


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

#test("hello".split(//),      ["h", "e", "l", "l", "o"], "PA split G")
#test("hello".split(//, 3),   ["h", "e", "llo"],         "PA split H")
# test("hi mom".split(/\s*/),  ["h", "i", "m", "o", "m"], "PA split I")
# test("".split,               [],                        "PA split J")

test "mellow yellow".split("ello"), ["m", "w y", "w"], "PA split J"

test "1,2,,3,4,,".split(','),     ["1", "2", "", "3", "4"], "PA split K"
test "1,2,,3,4,,".split(',',4),   ["1", "2", "", "3,4,,"],  "PA split L"
#test "1,2,,3,4,,".split(',', -4), ["1", "2", "", "3", "4", "", ""], "PA split M"

report
