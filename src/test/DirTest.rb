require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

d = Dir.new('/tmp')

# The following tests are order dependent (i.e., they depend on the
# internal state of the Dir object), so don't change the order.
test(d.read,     '.', 'read A')
test(d.read,    '..', 'read B')

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
  all_files =     ['a.rb', 'b.rb', 'deeper', 'README']
  all_dot_files = ['.', '..', '.rbinit' ] + all_files

  test(Dir.glob("*"), all_files, 'Dir.glob("*")')
  test(Dir.glob("*", File::FNM_DOTMATCH), all_dot_files, 'Dir.glob FNM_DOTMATCH')

  test(Dir.glob("[ax].rb"),        ['a.rb'],               'Dir.glob with []')
  # TODO: Fix bug and uncomment
  #test(Dir.glob("{a,b}.rb"),       ['a.rb', 'b.rb'],       'Dir.glob with {}')
  test(Dir.glob("**/even_deeper"), ['deeper/even_deeper'], 'Dir.glob with **')
end

report
