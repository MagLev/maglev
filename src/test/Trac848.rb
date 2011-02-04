# Found in Devise, (Devise::ALL)
class C
  def initialize(context)
    @context = context
  end

  def method_missing(method, *args, &block)
    puts "--- #{self}.method_missing(#{method}, #{args.inspect},...): #{@context}"
    @context.__send__(method, *args, &block)
  end
end

class D
  def foo(m, opts = { })
    raise "Fail: m should be a sym but was #{m.inspect}" unless Symbol === m
  end
end


d = D.new

c1 = C.new(d)
c2 = C.new(c1)
c2.foo(:a_sym, { :foo => :bar })
