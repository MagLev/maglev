require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

test(File.dirname('/home/gumby') , '/home' , "dirnameA")

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

# A couple of regressions:
test(File.basename('', ''),           '',     'File.basename regression 1')
test(File.basename('/'),              '/',    'File.basename regression 2')
test(File.basename('//'),             '/',    'File.basename regression 3')
test(File.basename('//dir///base//'), 'base', 'File.basename regression 4')

# Tests for extname

test(File.extname('test.rb'),       '.rb', 'Pickaxe extname A')
test(File.extname('a/b/d/test.rb'), '.rb', 'Pickaxe extname B')
test(File.extname('test'),          '',    'Pickaxe extname C')

test(File.extname('test.123'),      '.123', 'GemStone extname A')
test(File.extname('test.'),         '',     'GemStone extname B')
test(File.extname('test. '),        '. ',   'GemStone extname C')  # ?!!h


# Test stat based methods

# First create a file with known properties
fname = '/tmp/FileStatTest-234'
time = Time.local(1999, 10, 20, 12, 34)
%x{ touch -t 199910201234 #{fname} }  # create at Wed Oct 20 12:34:00 1999

# Test File class methods

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

test(File.exist?('/This/Better/Not/Exist'), false, 'File.exist? A')
test(File.exist?('/tmp'),                   true,  'File.exist? B')

test(File.exists?('/This/Better/Not/Exists'), false, 'File.exists? A')
test(File.exists?('/tmp'),                    true,  'File.exists? B')


test(File.expand_path("~"), ENV['HOME'],       'expand_path A')
test(File.expand_path("/foo/bar/.."), '/foo',  'expand_path B')
test(File.expand_path("./foo/bar/..", "/tmp"), '/tmp/foo', 'expand_path C')
test(File.expand_path(".//foo///bar//..", "/tmp"), '/tmp/foo', 'expand_path D')

#test(File.expand_path("///foo/bar/.."), '/foo',  'expand_path B')

# Test File instance methods
my_file = File.new(fname)

test(my_file.atime, time, 'File#atime')
test(my_file.mtime, time, 'File#mtime')
test(my_file.path, fname, 'File#path')

# This test case was causing mspec to fail.  Updated to
# rubinius version of File.join and it is now fixed.
result = File.join('spec/rubyspec/language', "/**/*_spec.rb")
test(result, 'spec/rubyspec/language/**/*_spec.rb', "File.join:  MSPECs are probably broken now...")

File.open(File.dirname(__FILE__)+'/lib/testfile') do |f|
  test(f.pos, 0, 'MISC pos at start')
  test(f.readline, "This is line one\n", 'MISC readline')
  test(f.pos, 17, 'MISC pos after readline')
  test(f.pos = 20, 20, 'MISC set pos to 20')
  test(f.pos, 20, 'MISC pos after pos = 20')
  test(f.readline, "s is line two\n", 'MISC readline after pos=')
end

# Test FileTest module methods
s = FileTest.size(__FILE__)
test(s > 100, true, 'FileTest.size(__FILE__)')


# The mac had a problem opening a file with these perms (the
# underlying primitive fdopen was using "a+" rather than "a".
log = open(__FILE__, (File::WRONLY | File::APPEND))
log.close
# This one raised a bogus error:
# error , TypeError,  'both APPEND and CREAT specified',
#           during /Users/pmclain/projects/maglev/git/src/test/FileTest.rb
# ERROR 2508, TypeError,  'both APPEND and CREAT specified'TypeError,  'both APPEND and CREAT specified'
log = open(__FILE__, (File::WRONLY | File::APPEND | File::CREAT))
log.close

# Another way to open files we need to support (from fileutils.rb)
fname = __FILE__ + 'x'
log = open(fname, 'wb', 0644)
log.close
File.delete(fname) if File.exist?(fname)

test(!!File::SYNC, true, "File::SYNC should not be nil")

#Test identical? for two files. This should not throw weird errors if one file does not exist
test(File.identical?(__FILE__, __FILE__), true, "a file should be equal to itself")
test(File.identical?(__FILE__, File.expand_path('FileStatTest.rb', File.dirname(__FILE__))), false, "files should not be identical")
test(File.identical?(__FILE__, File.expand_path('iAmANonExistentFile', File.dirname(__FILE__))), false, "files that do not exist are not equal to existing files")

report
