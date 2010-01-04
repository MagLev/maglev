
$failed = []
$count = 0
def test(actual, expected, msg)
  puts "==== Testing: #{msg}"
  $count += 1
  register_failure(msg, expected, actual) unless (expected == actual)
end

def report
  num = $failed.size
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  $failed.each { |f| puts f }
  raise "Failed #{$failed.size} tests" unless $failed.empty?
  true
end

def failed_test(msg, expected, actual)
  $count += 1
  register_failure(msg, expected, actual)
end

def register_failure(msg, expected, actual)
  emsg = "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}"
  $failed << emsg
  puts emsg
  unless ENV['SIMPLE_NO_PAUSE']  # don't pause if the env says not to...
    nil.pause if defined? RUBY_ENGINE # Keep MRI from trying to pause
  end
end
