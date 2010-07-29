# This crash is more specific than make_crash3.
class Foo
  attr_reader :data
  def initialize(b)
    data = b
  end
end
array = [ Foo.new('test1'), Foo.new('test2') ]

# Copied from Enumerable, with "obj" parameter added to operate on
def enum_find(obj, ifnone=nil, &block)
  unless block_given?
    return FirstEnumerator.new(self, :find, ifnone) # for 1.8.7
  end
  obj.each { |o| return o if block.call(o) }
  ifnone.call if ifnone
end

array.find { |x| false }

# Emulate the behavior of Maglev's Enumerable class
# to do a find()
#result = tree.find { |val| val.data == 'test3' }
result = enum_find(array) { |val| val.data == 'test3' }

puts "result: #{result}, class: #{result.class}"
