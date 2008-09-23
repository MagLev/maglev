
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

#  String#upcase tests
test "hEllO".upcase, "HELLO", "Pickaxe upcase A"
test "12!$".upcase,  "12!$",  "Gemstone upcase A"
s = "HELLO"
test s.upcase, s, "Gemstone upcase B"

#  String#upcase! tests
s = "hEllO"
test s.upcase!, "HELLO", "Pickaxe upcase A1"
test s, "HELLO",         "Pickaxe upcase A2"  # ensure modified in place

s = "HELLO"
test s.upcase!, nil,     "Gemstone upcase A1" # already uppercase, returns nil
test s, "HELLO",         "Gemstone upcase A2" # and leaves in place


s = "12!$"
test "12!$".upcase!,  nil,  "Gemstone upcase B1" # nothing to change => nil




#  String#downcase tests
test "hEllO".downcase, "hello", "Pickaxe downcase A"
test "12!$".downcase,  "12!$",  "Gemstone downcase A"

s = "hello"
test s.downcase, s, "Gemstone downcase B"

#  String#downcase! tests
s = "hEllO"
test s.downcase!, "hello", "Pickaxe downcase A1"
test s, "hello",           "Pickaxe downcase A2"  # ensure modified in place

s = "hello"
test s.downcase!, nil,   "Gemstone downcase A1" # already lowercase => nil
test s, "hello",         "Gemstone downcase A2" # and leaves in place


s = "12!$"
test "12!$".downcase!, nil, "Gemstone downcase B1" # nothing to change => nil

report
