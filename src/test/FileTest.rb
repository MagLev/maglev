
# require File.expand_path('simple', File.dirname(__FILE__))
$failed = []
$count = 0
def test(actual, expected, msg)
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
puts "========== A"
test(File.basename('/home/gumby/work/ruby.rb'),        'ruby.rb', "Pickaxe A")
puts "========== B"
test(File.basename('/home/gumby/work/ruby.rb', '.rb'), 'ruby',    "Pickaxe B")
puts "========== C"
test(File.basename('/home/gumby/work/ruby.rb', '.*'),  'ruby',    "Pickaxe C")
puts "========== D"


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


report
