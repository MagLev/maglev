require File.expand_path('simple', File.dirname(__FILE__))

r = ('A'..'D')
results = []
r.each { |i| results << i }

test(results, ['A', 'B', 'C', 'D'], "('A'..'D') test")

report
