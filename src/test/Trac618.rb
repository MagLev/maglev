$ This test most likely passes if it doesn't throw an error.
trace_var :$x, proc{puts "$x is now #{$x}"}
$x = 5
$x = 10
