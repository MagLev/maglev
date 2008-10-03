
# require File.expand_path('simple', File.dirname(__FILE__))
$failed = []
$count = 0
def test(actual, expected, msg)
  #puts "=== Testing #{msg}"
  $count += 1
  $failed << "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}" unless expected == actual
end

def report
  puts "=== Ran #{$count} tests.  Failed: #{$failed.size}"
  puts $failed
  raise $failed.join("\n") unless $failed.empty?
end

#     BEGIN TEST CASES

# Tests for basename
test(File.basename('/home/gumby/work/ruby.rb'),        'ruby.rb', "Pickaxe A")
test(File.basename('/home/gumby/work/ruby.rb', '.rb'), 'ruby',    "Pickaxe B")
test(File.basename('/home/gumby/work/ruby.rb', '.*'),  'ruby',    "Pickaxe C")

test(File.basename('ruby.rb',  '.*'),  'ruby',      "GemStone A")
test(File.basename('/ruby.rb', '.*'),  'ruby',      "GemStone B")
test(File.basename('ruby.rbx', '.*'),  'ruby',      "GemStone C")
test(File.basename('ruby.rbx', '.rb'), 'ruby.rbx',  "GemStone D")

test(File.basename('ruby.rb', ''),      'ruby.rb',  "GemStone E")
test(File.basename('ruby.rbx', '.rb*'), 'ruby.rbx', "GemStone F")
test(File.basename('ruby.rbx'), 'ruby.rbx',         "GemStone G")

# Try some extensions w/o a '.'
test(File.basename('ruby.rbx', 'rbx'), 'ruby.',     "GemStone H")
test(File.basename('ruby.rbx', 'x'),   'ruby.rb',   "GemStone I")
test(File.basename('ruby.rbx', '*'),   'ruby.rbx',  "GemStone J")

# Tests for extname

test(File.extname('test.rb'),       '.rb', 'Pickaxe extname A')
test(File.extname('a/b/d/test.rb'), '.rb', 'Pickaxe extname B')
test(File.extname('test'),          '',    'Pickaxe extname C')

test(File.extname('test.123'),      '.123', 'GemStone extname A')
test(File.extname('test.'),         '',     'GemStone extname B')
test(File.extname('test. '),        '. ',   'GemStone extname C')  # ?!!h


# Test stat based methods

# First create a file with known properties
fname = "/tmp/FileStatTest-234"
time = Time.at 940448040              # Wed Oct 20 12:34:00 -0700 1999
%x{ touch -t 199910201234 #{fname} }  # create at Wed Oct 20 12:34:00 1999

test(File.atime(fname), time, 'File.atime')
test(File.blockdev?(fname), false, 'File.blockdev?')
test(File.chardev?(fname), false, 'File.chardev?')
#test(File.ctime(fname), , 'File.')
test(File.directory?(fname), false, 'File.directory?')
test(File.file?(fname), true, 'File.file?')
test(File.mtime(fname), time, 'File.mtime')
test(File.pipe?(fname), false, 'File.pipe?')
test(File.size(fname), 0, 'File.size')
test(File.size?(fname), nil, 'File.size?')
test(File.socket?(fname), false, 'File.socket?')
test(File.sticky?(fname), false, 'File.sticky?')
test(File.symlink?(fname), false, 'File.symlink?')
test(File.zero?(fname), true, 'File.zero?')

report
