# file Trac883b.rb ,  additional coverage for Trac 883
module M
  protected
  def protected_meth_from_module
    12
  end
end

class X
  include M

  def call_protected_meth_on(x)
    puts "#{self} calling protected_meth on #{x}"
    x.protected_meth

    puts "#{self} calling protected_meth_from_module on #{x}"
    x.protected_meth_from_module   # without fix 883, trouble here
  end

  def call_private_meth_on_self
    private_meth
  end

  protected
  def protected_meth
    10
  end

  private
  def private_meth
    puts "Private method called"
    11
  end
end


# Self can always call all methods
x1 = X.new
x1.call_protected_meth_on(x1)
x1.call_private_meth_on_self

begin
  x1.private_meth    # but this raises error because receiver is declared
  raise "Fail: expected NoMethodError"
rescue NoMethodError => e
  puts "OK"
end


x2 = X.new
x2.call_protected_meth_on(x1)

class Y < X
end

y = Y.new

y.call_protected_meth_on(x1)
x1.call_protected_meth_on(y)


