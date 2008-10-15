
# require File.expand_path('simple', File.dirname(__FILE__))
$failed = []
$count = 0
def test(actual, expected, msg)
  #puts "=== Testing #{msg}"
  $count += 1
  $failed << "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}" unless expected == actual
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  puts $failed
  raise $failed.join("\n") unless $failed.empty?
end

#     BEGIN TEST CASES

d = Dir.new('/tmp')

# The following tests are order dependent (i.e., they depend on the
# internal state of the Dir object), so don't change the order.
test(d.read,     '.', 'read A')
test(d.read,    '..', 'read B')
test(d.pos,        2, 'pos A')
test(d.pos = 0,    0, 'pos= A')
test(d.tell,       0, 'tell A')

report
