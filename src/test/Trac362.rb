$v = 0
$m = Module.new do
  def foo
    $v = 27
  end
end

class Bar
  include $m
end

str = $m.inspect
unless str[0, 11] == "#<Module:0x" ; raise 'error;' end # coverage for Trac 650
Bar.new.foo
unless $v == 27 ; raise 'err'; end
true
