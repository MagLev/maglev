class Super
  attr_reader :block, :size
  def initialize(size, &blk)
    gv = block_given?
    @block = gv ? blk : Proc.new { :no_block_given }
    @size = size
  end
end

class Derived < Super
  def initialize(kxx, &block)
    gv = block_given?
    bxx = if gv
          Proc.new { :not_block_from_derived }
        else
          Proc.new { :default_from_derived }
        end
    super(kxx, &bxx)
  end
end


d = Derived.new(3)
expected = :default_from_derived
result = d.block.call
puts "d.block.call =  #{result}"
unless result == expected
  raise "Fail: expecting :default_from_derived not #{result.inspect}" 
end
true
