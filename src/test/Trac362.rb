$v = 0
$m = Module.new do
  def foo
    $v = 27
  end
end

class Bar
  include $m
end

Bar.new.foo
unless $v == 27 ; raise 'err'; end
true
