# A regression from the norvig benchmark  Trac
require File.expand_path('simple', File.dirname(__FILE__))

word = 'speling'
i = word.length

test(word[(i-1)..-1], 'g', 'Test [n-1..-1]')
test(word[i..-1],      '', 'Test [n..-1]')     # <=== this was returning nil...
test(word[(i+1)..-1], nil, 'Test [n+1..-1]')
#################### Trac Info
# ID:         430
# Summary:    Coercion error in new RBS Norvig spelling benchmark
# Changetime: 2009-04-13 20:48:00+00:00
###

#  Antonio is adding benchmarks to the ruby-benchmark-suite. This one fails.
#  
#  {{{
#  maglev-ruby -d norvig_spelling.rb
#  ERROR 2023, Error, 'Coercion error: nil.to_str => String failed:
#  (Undefined method `to_str'' for   )'
#  }}}
#  
#  Attached zipfile contains norvig_spelling.rb and holmes.txt