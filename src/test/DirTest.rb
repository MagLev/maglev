require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

d = Dir.new('/tmp')

# The following tests are order dependent (i.e., they depend on the
# internal state of the Dir object), so don't change the order.
#
test(d.read.class, String, 'read A') # Usually '.', but not on ubuntu
test(d.read.class, String, 'read B') # Usually '..', but not on ubuntu

# TODO: ruby fails on this one (returns 1 rather than 2): why?!
#       if I manually do two reads and a pos in irb, it shows 2...
test(d.pos,        2, 'pos A')

test(d.pos = 0,    0, 'pos= A')
test(d.tell,       0, 'tell A')

# Ensure enumerable is included properly
test(d.select { |e| e == "." || e == ".."}, [".", ".."], 'Enumerable: select')

# The following tests depend on the pre-defined files in test_dir
# The constants below are the exepcted names of files in them
test_dir = File.join(File.dirname(__FILE__), 'test_dir')
Dir.chdir(test_dir) do
  all_files =     [ 'README', 'a.rb', 'b.rb', 'deeper']
  all_dot_files = ['.', '..', '.rbinit' ] + all_files

  test(Dir.glob("*").sort , all_files, 'Dir.glob("*")')
  test(Dir.glob("*", File::FNM_DOTMATCH).sort, all_dot_files, 'Dir.glob FNM_DOTMATCH')

  test(Dir.glob("[ax].rb"),        ['a.rb'],               'Dir.glob with []')
  # TODO: Fix bug and uncomment
  test(Dir.glob("{a,b}.rb").sort,       ['a.rb', 'b.rb'],       'Dir.glob with {}')
  test(Dir.glob("**/even_deeper"), ['deeper/even_deeper'], 'Dir.glob with **')
end

# #############################################
# A regression found in the Rails Generators.
base   = File.join(File.dirname(__FILE__), 'test_dir')
lookup = File.join(base,  '**' , '{*,.[a-z]*}')

# Do the Dir glob, but remove the path prefix: /..../src/test/
x = File.dirname(__FILE__).length + 1
actual = Dir[lookup].map { |f| f.slice(x..-1)}.sort

expected = ["test_dir/.rbinit", "test_dir/README", "test_dir/a.rb", "test_dir/b.rb",
            "test_dir/deeper", "test_dir/deeper/even_deeper"]
test(actual, expected, 'Dir[] with complex pattern: size')

# #########################
# Test Dir#each
d = Dir.open('.')
results = []
d.each { |f| results << f }
test(results.length > 0, true, 'Dir#each')


report
