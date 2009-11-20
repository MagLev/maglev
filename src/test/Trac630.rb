# From minitest
module Kernel
  def describe(desc, &block)
    stack = MiniTest::Spec.describe_stack
    name = desc.to_s.capitalize
    cls = Object.class_eval "class #{name} < #{stack.last}; end; #{name}"
    block.call
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
cx = describe :foo do
  $b = 5
end
unless $b == 5 ; raise 'error'; end
unless cx.name == 'Foo' ; raise 'error'; end
true
