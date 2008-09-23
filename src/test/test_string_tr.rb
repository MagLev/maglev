# Test the logic of String#tr, String#tr!, String#tr_s and String#tr_s!

$failed = []
$count = 0
def test(actual, expected, msg)
    $count += 1
    $failed << "ERROR: #{msg} Expected: #{expected} actual: #{actual}" unless expected == actual
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  puts $failed
end


#     BEGIN TEST CASES

#  tr tests from Pickaxe
s = "hello"
test s.tr('aeiou', '*'),  'h*ll*', "Pickaxe tr A"
test s.tr('^aeiou', '*'), '*e**o', "Pickaxe tr B"
test s.tr('el', 'ip'),    'hippo', "Pickaxe tr C"
test s.tr('a-y', 'b-z'),  'ifmmp', "Pickaxe tr D"

s = "123ABCabc-^"
test s.tr('^123', 'abc'), "123cccccccc", "Gemstone tr A"

#  tr_s tests from Pickaxe
s = "hello"
test s.tr_s('l', 'r'),   'hero', "Pickaxe tr_s A"
test s.tr_s('el', '*'),  'h*o',  "Pickaxe tr_s B"
test s.tr_s('el', 'hx'), 'hhxo', "Pickaxe tr_s C"

# Make sure repeated compactions happen
test "aaaBaaaCaa".tr_s('a', '-'),  '-B-C-', "Gemstone tr_s A"
test "aaaBbbbCaa".tr_s('ab', '-'), '-B-C-', "Gemstone tr_s B"
test '12345'.tr_s('a-z', '*'), '12345',     "Gemstone tr_s C"

#  tr_s! tests
test "hello".tr_s!('l', 'r'),  'hero', "Pickaxe tr_s! A"
test "hello".tr_s!('el', '*'),  'h*o',  "Pickaxe tr_s! B"
test "hello".tr_s!('el', 'hx'), 'hhxo', "Pickaxe tr_s! C"

test "aaaBaaaCaa".tr_s!('a', '-'),  '-B-C-', "Gemstone tr_s! A"
test "aaaBbbbCaa".tr_s!('ab', '-'), '-B-C-', "Gemstone tr_s! B"
test '12345'.tr_s!('a-z', '*'), nil,         "Gemstone tr_s! C"

report
