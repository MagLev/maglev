class C
  def eval(an_arg)
      $_ = '998'
      Kernel.eval(an_arg)
  end
  def self.test(arg)
    self.new.eval(arg)
  end
end
class C918
  def eval()
    # coverage for Trac 918
  end
end

x = C.test("987 + 10")
unless x == 997 ; raise 'error'; end
true

