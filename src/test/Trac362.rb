$m = Module.new do
  def foo
    puts "new module foo"
  end
end

class Bar
  include $m
end

Bar.new.foo
