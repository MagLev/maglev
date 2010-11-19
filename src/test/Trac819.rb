# MRI prints out the file and line number with the error:
# 
#   $ ruby ~/tmp/l.rb 
#   /Users/pmclain/tmp/l.rb:2: syntax error, unexpected tINTEGER, expecting $end
#   x << Array.new 4 if 1 == 2
# 
# MagLev does not print the file info:
# 
#   $ maglev-ruby ~/tmp/l.rb 
#   at line 2, syntax error, found tINTEGER  expected EOF end-of-file 
#   ERROR 2023, Error, 'at line 2, syntax error, found tINTEGER  expected EOF end-of-file 
#   unexpected EOF at line 3' (SyntaxError)
#
x = Array.new
x << Array.new 4 if 1 == 2

