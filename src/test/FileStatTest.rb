
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


fname = "/tmp/FileStatTest-234"
time = Time.at 940448040              # Wed Oct 20 12:34:00 -0700 1999
%x{ touch -t 199910201234 #{fname} }  # create at Wed Oct 20 12:34:00 1999
file = File.open fname
stat = file.stat

test(stat.atime, time, 'atime') # TODO: Time needs ==
test(stat.blockdev?, false, 'blockdev?')
test(stat.blocks, `ls -s #{fname}`.split[0].to_i, 'blocks')
test(stat.chardev?, false, 'chardev?')
#test(stat.ctime, time, 'ctime')  # no good way to get creation time...
test(stat.dev, 234881026, 'dev')

# TODO: need dynamic way of getting expected values for the current
# platform dev_major dev_minor
test(stat.dev_major, 14, 'dev_major')
test(stat.dev_minor, 2, 'dev_minor')

test(stat.directory?, false, 'directory?')
#test(stat.executable?, false, 'executable?')
#test(stat.executable_real?, false, 'executable_real?')
test(stat.file?, true, 'file?')
test(stat.ftype, 'file', 'ftype')
test(stat.gid, `ls -ln #{fname}`.split[3].to_i, 'gid')
#test(stat.grpowned?, '', 'grpowned?')
test(stat.ino, `ls -i #{fname}`.split[0].to_i, 'ino')
test(stat.mode, 33188, 'mode')  # TODO: depends on umask....
test(stat.mtime, time, 'mtime')
test(stat.nlink, `ls -l #{fname}`.split[1].to_i, 'nlink')
#test(stat.owned?, '', 'owned?')
#test(stat.pipe?, '', 'pipe?')
test(stat.rdev, 0, 'rdev')

# TODO: need dynamic way of getting expected values for the current
# platform rdev_major rdev_minor
test(stat.rdev_major, 0, 'rdev_major')
test(stat.rdev_minor, 0, 'rdev_minor')

#test(stat.readable?, true, 'readable?')
#test(stat.readable_real?, true, 'readable_real?')
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
#test(stat.writable?, true, 'writable?')
#test(stat.writable_real?, true, 'writable_real?')
test(stat.zero?, true, 'zero?')

# Test the comparable methods
same_stat = stat
other_stat = File.open(__FILE__).stat
test(stat <=> stat, 0, 'stat <=> stat')
test(stat <=> same_stat, 0, 'stat <=> same_stat')
test(stat <=> other_stat, -1, 'stat <=> other_stat')

report
