# From minitest
module MX
  AX = 9930
  def self.describe(desc, &block)
    stack = MiniTest::Spec.describe_stack
    name = desc.to_s.capitalize
    cls = Object.class_eval "$y = AX ; class #{name} < #{stack.last}; end; #{name}"
    unless $b == 0 ; raise 'error'; end
    block.call
    unless $b == 5 ; raise 'error'; end
    cls
  end
end

module MiniTest
end

class MiniTest::Spec
  @@describe_stack = [MiniTest::Spec]
  def self.describe_stack
    @@describe_stack
  end
end

$b = 0
cx = MX::describe( :foo ) { $b = 5 }

unless $b == 5 ; raise 'error'; end
puts cx.inspect
unless (nx = cx.name) == 'Foo' ; raise 'error'; end
unless $y == 9930 ; raise 'error'; end
true
