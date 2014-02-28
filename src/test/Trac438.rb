# coverage for trac 438 and 442

class Test438
  def test(*t)
  end
  def foo(a,b)
  end
end
m = Test438.instance_method(:test)
mf = Test438.instance_method(:foo)
ma = m.arity
mfa = mf.arity
unless ma == -1 ; raise 'error'; end;
unless mfa == 2 ; raise 'error'; end

acd = Module.method(:const_defined?).arity

unless acd == -1 ; raise "error #{acd}"; end;
