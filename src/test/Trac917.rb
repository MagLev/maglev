class A
  def setup_blocks(base = self.class)
    $aa <<  base
    # puts "in setup_blocks for base #{base}"
    setup_blocks(base.superclass) unless base.superclass.nil?
  end
  alias setup setup_blocks
end

class B < A
  def setup
    super
  end
end

$aa = []
B.new.setup

unless $aa == [B, A, Object] ; raise 'fail'; end
true
