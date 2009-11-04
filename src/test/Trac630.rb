# From minitest
module Kernel
  def describe desc, &block
    stack = MiniTest::Spec.describe_stack
    name = desc.to_s.capitalize
    cls = Object.class_eval "class #{name} < #{stack.last}; end; #{name}"
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

describe :foo do
  puts "whatever"
end
