require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

plat = RUBY_PLATFORM

if plat.index('solaris')
  # use xpg4 compatibility version
  test(Maglev::System.getgid,  `/usr/xpg4/bin/id -r -g`.chomp.to_i, "getgid")
  test(Maglev::System.getegid, `/usr/xpg4/bin/id -g`.chomp.to_i,    "getegid")

  test(Maglev::System.getuid,  `/usr/xpg4/bin/id -r -u`.chomp.to_i, "getuid")
  test(Maglev::System.geteuid, `/usr/xpg4/bin/id -u`.chomp.to_i,    "geteuid")
else
  test(Maglev::System.getgid,  `id -r -g`.chomp.to_i, "getgid")
  test(Maglev::System.getegid, `id -g`.chomp.to_i,    "getegid")

  test(Maglev::System.getuid,  `id -r -u`.chomp.to_i, "getuid")
  test(Maglev::System.geteuid, `id -u`.chomp.to_i,    "geteuid")
end
report
