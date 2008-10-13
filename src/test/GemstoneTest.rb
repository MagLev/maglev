
$failed = []
$count = 0
def test(actual, expected, msg)
  puts "==== Testing: #{msg}"
  $count += 1
  $failed << "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}" unless expected == actual
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  $failed.each { |f| puts f }
  raise "Failed #{$failed.size} tests" unless $failed.empty?
end

#     BEGIN TEST CASES

test(Gemstone.getgid,  `id -r -g`.chomp.to_i, "getgid")
test(Gemstone.getegid, `id -g`.chomp.to_i,    "getegid")

test(Gemstone.getuid,  `id -r -u`.chomp.to_i, "getuid")
test(Gemstone.geteuid, `id -u`.chomp.to_i,    "geteuid")

report
