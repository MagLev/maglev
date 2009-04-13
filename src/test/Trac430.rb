# A regression from the norvig benchmark  Trac
require File.expand_path('simple', File.dirname(__FILE__))

word = 'speling'
i = word.length

test(word[(i-1)..-1], 'g', 'Test [n-1..-1]')
test(word[i..-1],      '', 'Test [n..-1]')     # <=== this was returning nil...
test(word[(i+1)..-1], nil, 'Test [n+1..-1]')
