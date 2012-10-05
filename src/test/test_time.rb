require File.expand_path('simple', File.dirname(__FILE__))

t  = Time.local(2008, 10, 01, 14, 03, 42)  # Wed Oct 01 14:03:42 -0700 2008
t2 = Time.local(2008, 10, 01, 14, 03, 42)
later = Time.local(2008, 10, 01, 14, 03, 43)

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
# test(t.strftime("%c"), "Wed Oct  1 14:03:42 2008", "%c test")
test(t.strftime("%d"), "01",        "%d test")
test(t.strftime("%H"), "14",        "%H test")
test(t.strftime("%I"), "02",        "%I test")
test(t.strftime("%j"), "275",       "%j test")
test(t.strftime(
  "                                                           %j"),
  "                                                           275",  "%j test")
test(t.strftime("%m"), "10",        "%m test")
test(t.strftime("%M"), "03",        "%M test")
test(t.strftime("%p"), "PM",        "%p test")
test(t.strftime("%S"), "42",        "%S test")
test(t.strftime("%U"), "39",        "%U test")
test(t.strftime("%W"), "39",        "%W test")
test(t.strftime("%w"), "3",         "%w test")
test(t.strftime("%y"), "08",        "%y test")
test(t.strftime("%Y"), "2008",      "%Y test")
# test(t.strftime("%Z"), "PDT",       "%Z test")
test(t.strftime("%%"), "%",         "%% test")
# %x returns 2008 on Linux, 08 on Solaris
# test(t.strftime("%x"), "10/01/08",  "%x test")

# Test equality of Time objects
test(t.equal?(t), true, 't.equal t')
test(t.equal?(t2), false, 't.equal? t2')
test(t.equal?(later), false, 't.equal? later')
test(t.equal?('hello'), false, 't.equal? "hello"')

test(t.eql?(t), true, 't.eql? t')
test(t.eql?(t2), true, 't.eql? t2')
test(t.eql?(later), false, 't.eql? later')
test(t.eql?('hello'), false, 't.eql? "hello"')

test(t == t, true, 't == t')
test(t == t2, true, 't == t2')
test(t == later, false, 't == later')
test(t == 'hello', nil, 't == "hello"')

report
