
$failed = []
$count = 0
def test(actual, expected, msg)
  puts "==== Testing: #{msg}"
  $count += 1
  if (expected == actual)
    # ok
  else
    $failed << "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}" 
    self.pause
  end
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  $failed.each { |f| puts f }
  raise "Failed #{$failed.size} tests" unless $failed.empty?
end

