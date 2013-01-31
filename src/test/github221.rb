class SomeClass
  def method_missing(*args, &block)
    shouldBe = [:x, 4, 5, 1, 2, 3]
    if not args == shouldBe
      raise "args should be #{shouldBe}, but was #{args}" 
    end
  end
end

a = [1, 2, 3]
SomeClass.new.x(4, 5, *a)

