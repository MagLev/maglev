=begin
# This fails with the native parser
=end

=begin
  more dead code
=end

puts "Test succeeded"
true
#################### Trac Info
# ID:         572
# Summary:    new parser doesn't handle =begin
# Changetime: 2009-07-30 20:26:04+00:00
###

#  Works in old parser and MRI but fails in new parser. YARV benchmarks won't run as lib/ruby/1.8/benchmark.rb has this construct.
#  
#  {{{
#  =begin
#  # This fails with the native parser
#  =end
#  
#  puts "Test succeeded"
#  }}}
#  
#  {{{
#  $ maglev-ruby RDoc.rb 
#  parse error on value (RpNameToken = @0) nil 
#  SyntaxError: missing or unexpected (RpNameToken = @0) , near line 1 
#  error , SyntaxError,
#            during /congo1/users/monty/MagLev/Tests/RDoc.rb
#  ERROR 2023, Error, 'SyntaxError'
#  }}}
#  
#  
#  