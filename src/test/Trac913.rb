class C
  define_method( 'foo:bar:' ) { | x | $ax = x }
end

C.new.send('foo:bar:', 'today913')
unless $ax == 'today913' ; raise 'fail' ; end

class D
end
D.send(:define_method, "foo:xx") { |x| $bx << x }

$bx = []

mx = D.instance_methods(false)

D.new.send("foo:xx")
D.new.send("foo:xx" , 99)

bx = $bx
unless bx == [ nil, 99 ] ; raise 'fail'; end
puts mx.inspect
puts "ok"
