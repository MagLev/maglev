class Fiber
  @@fs = []

  def initialize(&block)
    @k = lambda(&block) # lambda makes return work correctly
    @prev = nil
  end

  def resume(*xs)
    raise FiberError, "dead fiber called" if @dead
    raise FiberError, "double resume" if @@fs[-1] == self
    @@fs.push(self)
    jump(xs)
  end

  def self.current
    @@fs.last
  end

  def self.yield(*xs)
    f = @@fs.pop
    f && f.send(:jump, xs)
  end

  def transfer(*xs)
    raise NotImplementedError
  end

  def alive?
    !@dead
  end

  private
  def jump(xs)
    callcc do |k|
      destination = @k
      @k = k
      result = destination.call(*xs)
      @dead = true
      @@fs.pop
      @k.call result
    end
  end
end

class FiberError < StandardError; end
