# From RSpec

superclass = ['::StandardError', 'Test::Unit::AssertionFailedError'].map do |c|
  eval(c) rescue nil
end
p superclass

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


