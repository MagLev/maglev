# This test most likely passes if it doesn't throw an error.
trace_var :$x, proc{puts "$x is now #{$x}"}
$x = 5
$x = 10

#  more tests
$X = 0
trace_var( '$A') { |v| $X = v; }
$A = 9
unless $X == 9 ; raise 'error'; end
$Y = 0
trace_var( :$A ) { |v| $Y = v; }
$A = 25
unless $X == 25 ; raise 'error'; end
unless $Y == 25 ; raise 'error'; end
arr = untrace_var( :$A )
unless arr.size == 2 ; raise 'error'; end 
arr = untrace_var( :$A )
unless arr == [] ; raise 'error'; end   
begin
  untrace_var( '$W' )
rescue NameError
  $Y = 0
end
unless $Y == 0 ; raise 'error'; end
true
