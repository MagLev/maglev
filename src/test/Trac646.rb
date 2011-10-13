# Trac 646
require 'date'

s = Date.strptime("2010/1", "%Y/%W").to_s

unless s == '2010-01-10' ; raise 'error'; end
puts "ok"
true
#################### Trac Info
# ID:         646
# Summary:    Erroneous date for Monday of first week in 2010
# Changetime: 2010-03-16 15:56:26+00:00
###

#  Noticed when reading Ruby-Core about a Ruby 1.8 bug where the result was "2010-01-10" instead of "2010-01-04"
#  
#  {{{
#  $ maglev-ruby -v -rdate -e 'p Date.strptime("2010/1", "%Y/%W").to_s'
#  maglev 0.6 (ruby 1.8.6) (2010-01-04 rev 22683-1088) [Linux x86_64]
#  "8732-02-26"
#  }}}
#  
#  