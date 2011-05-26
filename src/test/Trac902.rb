class Trac902
  def foo=(bar)
  end
end

x = Trac902.new.foo=(43)
unless x == 43 ; raise 'fail'; end
true
