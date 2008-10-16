require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

test(Gemstone.getgid,  `id -r -g`.chomp.to_i, "getgid")
test(Gemstone.getegid, `id -g`.chomp.to_i,    "getegid")

test(Gemstone.getuid,  `id -r -u`.chomp.to_i, "getuid")
test(Gemstone.geteuid, `id -u`.chomp.to_i,    "geteuid")

report
