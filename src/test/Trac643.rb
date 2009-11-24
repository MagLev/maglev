class Super
  attr_reader :block, :size
  def initialize(size=nil, &block)
    @block = block_given? ? block : Proc.new { :no_block_given }
    @size = size
  end
end

class Derived < Super
  def initialize(k, &block)
    b = if block_given?
          Proc.new { :not_block_from_derived }
        else
          Proc.new { :default_from_derived }
        end
    super(k, &b)
  end
end


d = Derived.new(3)
expected = :default_from_derived
result = d.block.call
raise "Fail: expecting :default_from_derived not #{result.inspect}" unless
  result == expected

