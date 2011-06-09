class C
  define_method( 'foo:bar:' ) { | x | $ax = x }
end

C.new.send('foo:bar:', 'today913')
unless $ax == 'today913' ; raise 'fail' ; end
true
