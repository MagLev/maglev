class C
  def eval(an_arg)
    Kernel.eval(an_arg)
  end
end

C.new.eval "puts 10"

