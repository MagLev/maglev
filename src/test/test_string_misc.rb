# Misellaneous tests for string (e.g., regressions and random corner cases)

$failed = []
$count = 0
def test(actual, expected, msg)
    $count += 1
    $failed << "ERROR: #{msg} Expected: #{expected} actual: #{actual}" unless expected == actual
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

report
