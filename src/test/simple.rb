
$failed = []
$count = 0
def test(actual, expected, msg)
  puts "==== Testing: #{msg}"
  $count += 1
  register_failure(msg, expected, actual) unless (expected == actual)
end

def report
  num = $failed.size
  if ($failed.size > 0 && defined? RUBY_ENGINE)
    num.pause
  end
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
  x = $failed
  nil.pause if defined? RUBY_ENGINE # Keep MRI from trying to pause
end
