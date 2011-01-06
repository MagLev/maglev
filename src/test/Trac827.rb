
class Hook
  def initialize(&b)
    @block = b
  end

  def call
    @block.call
  end

  def to_proc
    @block
  end

  def run
    Object.new.instance_eval(&self)
  end
end


h = Hook.new { puts "Hi" }
h.run
