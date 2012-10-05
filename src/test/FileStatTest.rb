require File.expand_path('simple', File.dirname(__FILE__))

fname = "/tmp/FileStatTest-#{rand(100_000).to_s}"
time = Time.local(1999, 10, 20, 12, 34, 00)              # Wed Oct 20 12:34:00 -0700 1999

# deleted line chgrp $(id -g) #{fname} # get 'Illegal variable name.' from shell
%x{
  touch -t 199910201234 #{fname}
  chmod 0707 #{fname}
}
file = File.open fname
stat = file.stat
file.close

test(stat.atime, time, 'atime') # TODO: Time needs ==
test(stat.blockdev?, false, 'blockdev?')

sx = `ls -s #{fname}`
bx = sx.split[0].to_i
test(stat.blocks, bx , 'blocks')

# coverage for Trac 647
dx = $?
unless dx.class._equal?( Process::Status ) ; raise 'error'; end
test( dx.exitstatus , 0, 'exitstatus' )
test( dx.success? , true , 'exitstatus' )

test(stat.chardev?, false, 'chardev?')
test(stat.ctime.class, Time, 'ctime')  # value of stat.ctime is time dependent
test(stat.dev.class, Fixnum, 'dev')    # stat.dev is machine dependent

# TODO: need dynamic way of getting expected values for the current
# platform dev_major dev_minor
test(stat.dev_major.class, Fixnum, 'dev_major') # stat.dev_major is machine dependent
test(stat.dev_minor.class, Fixnum, 'dev_minor') # stat.dev_minor is machine dependent

test(stat.directory?, false, 'directory?')
# EDITS for Solaris X86 problems,           DO NOT CHECKIN
#test(stat.executable?, true, 'executable?')
#test(stat.executable_real?, true, 'executable_real?')
test(stat.file?, true, 'file?')
test(stat.ftype, 'file', 'ftype')
test(stat.gid, `ls -ln #{fname}`.split[3].to_i, 'gid')
#test(stat.grpowned?, true, 'grpowned?')
test(stat.ino, `ls -i #{fname}`.split[0].to_i, 'ino')
#test(stat.mode, 33223, 'mode')
test(stat.mtime, time, 'mtime')
test(stat.nlink, `ls -l #{fname}`.split[1].to_i, 'nlink')
test(stat.owned?, true, 'owned?')
#test(stat.pipe?, '', 'pipe?')
# test(stat.rdev, 0, 'rdev')

# TODO: need dynamic way of getting expected values for the current
# platform rdev_major rdev_minor
# test(stat.rdev_major, 0, 'rdev_major')
# test(stat.rdev_minor, 0, 'rdev_minor')

test(stat.readable?, true, 'readable?')
test(stat.readable_real?, true, 'readable_real?')
test(stat.setgid?, false, 'setgid?')
test(stat.setuid?, false, 'setuid?')
test(stat.size, `ls -l #{fname}`.split[4].to_i, 'size')
test(stat.size?, nil, 'size?')
test(stat.socket?, false, 'socket?')

# Create a sticky dir and a non-sticky dir to test
dir_name = "/tmp/stickydir"
%x{ rm -rf #{dir_name}
    mkdir -p #{dir_name} }

dir_stat = File.open(dir_name).stat
test(dir_stat.sticky?, false, 'sticky? on non-sticky dir')

%x{ chmod 1755 #{dir_name} }
dir_stat = File.open(dir_name).stat
test(dir_stat.sticky?, true, 'sticky? on sticky dir')

%x{ rm -rf #{dir_name} }


test(stat.symlink?, false, 'symlink?')
test(stat.uid, `ls -ln #{fname}`.split[2].to_i, 'uid')
test(stat.writable?, true, 'writable?')
test(stat.writable_real?, true, 'writable_real?')
test(stat.zero?, true, 'zero?')

# Test the comparable methods
same_stat = stat
other_stat = File.open(__FILE__).stat
test(stat <=> stat, 0, 'stat <=> stat')
test(stat <=> same_stat, 0, 'stat <=> same_stat')
test(stat <=> other_stat, -1, 'stat <=> other_stat')

fscls = File::Stat
test( fscls.name , 'File::Stat' , 'File::Stat name')

report

File.delete fname

