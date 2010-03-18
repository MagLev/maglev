# From RSpec

arr = ['::RangeError', '::ArgumentError'].map do |c|
  eval(c) rescue nil
end
unless arr == [ RangeError, ArgumentError ] ; raise 'error' ; end
puts arr.inspect
true

# module Foo
#   superclass = ['Test::Unit::AssertionFailedError', '::StandardError'].map do |c|
#     eval(c) rescue nil
#   end

#   p superclass
#   superclass = superclass.compact.first
#   p superclass

#   class ExpectationNotMetError < superclass
#   end
# end


