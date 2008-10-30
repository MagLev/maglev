class Object
  def taint
    raise TypeError, "Can't modify frozen object" if self.frozen?
    @tainted = true
  end

  def tainted?
    @tainted != nil && @tainted
  end

  def untaint
    raise TypeError, "Can't modify frozen object" if self.frozen?
    @tainted = false
  end
end
