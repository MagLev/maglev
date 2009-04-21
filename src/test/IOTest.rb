require File.expand_path('simple', File.dirname(__FILE__))

# Pickaxe tests
file = File.dirname(__FILE__) + '/lib/testfile'

test(IO.read(file), "This is line one\nThis is line two\nThis is line three\nAnd so on...\n", 'A')
test(IO.read(file, 20), "This is line one\nThi", 'B')
test(IO.read(file, 20, 10), "ne one\nThis is line ", 'C')


File.open(File.dirname(__FILE__)+'/lib/testfile') do |f|
  expected = [ "This is line one\n", "This is line two\n",
               "This is line three\n", "And so on...\n" ]
  lines = f.readlines
  test(lines, expected, 'readlines')
end

report
true
