
$failed = []
$count = 0
def test(actual, expected, msg)
  puts "==== Testing: #{msg}"
  $count += 1
  register_failure(msg, expected, actual) unless (expected == actual)
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  $failed.each { |f| puts f }
  raise "Failed #{$failed.size} tests" unless $failed.empty?
end

def failed_test(msg, expected, actual)
  $count += 1
  register_failure(msg, expected, actual)
end

def register_failure(msg, expected, actual)
  $failed << "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}"
end
