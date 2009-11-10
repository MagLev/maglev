require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

test(Maglev::System.getgid,  `id -r -g`.chomp.to_i, "getgid")
test(Maglev::System.getegid, `id -g`.chomp.to_i,    "getegid")

test(Maglev::System.getuid,  `id -r -u`.chomp.to_i, "getuid")
test(Maglev::System.geteuid, `id -u`.chomp.to_i,    "geteuid")

report
