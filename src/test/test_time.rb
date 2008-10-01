
$failed = []
$count = 0
def test(actual, expected, msg)
  puts "==== Testing: #{msg}"
  $count += 1
  $failed << "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}" unless expected == actual
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  puts $failed
  raise $failed.join("\n") unless $failed.empty?
end

t = Time.at(1222895022)  # Wed Oct 01 14:03:42 -0700 2008

test(t.year, 2008, "year()")
test(t.day,    1,  "day()")
test(t.hour,  14,  "hour()")
test(t.mday,   1,  "mday()")
test(t.min,    3,  "min()")
test(t.mon,   10,  "mon()")
test(t.month, 10,  "month()")
test(t.sec,   42,  "sec()")
test(t.wday,   3,  "wday(")
test(t.yday, 275,  "yday()")
test(t.year, 2008, "year()")
#test(t.zone, "PDT","zone()")

#test(t.to_a, [42, 3, 14, 1, 10, 2008, 3, 275, true, "PDT"],  "to_a")

test(t.strftime("%a"), "Wed",       "%a test")
test(t.strftime("%A"), "Wednesday", "%A test")
test(t.strftime("%b"), "Oct",       "%b test")
test(t.strftime("%B"), "October",   "%B test")
#test(t.strftime("%c"), "Wed Oct  1 14:03:42 2008", "%c test")
#test(t.strftime("%d"), "01",        "%d test") # TODO: Waiting on sprintf
test(t.strftime("%H"), "14",        "%H test")
#test(t.strftime("%I"), "02",        "%I test") # TODO: Waiting on sprintf
test(t.strftime("%j"), "275",       "%j test")
test(t.strftime("%m"), "10",        "%m test")
#test(t.strftime("%M"), "03",        "%M test") # TODO: Waiting on sprintf
test(t.strftime("%p"), "PM",        "%p test")
test(t.strftime("%S"), "42",        "%S test")
#test(t.strftime("%U"), "39",        "%U test")
#test(t.strftime("%W"), "39",        "%W test")
test(t.strftime("%w"), "3",         "%w test")
#test(t.strftime("%x"), "10/01/08",  "%x test") # TODO: Waiting on sprintf
test(t.strftime("%y"), "08",        "%y test")
test(t.strftime("%Y"), "2008",      "%Y test")
#test(t.strftime("%Z"), "PDT",       "%Z test")
test(t.strftime("%%"), "%",         "%% test")

report
