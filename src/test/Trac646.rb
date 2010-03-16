# Trac 646
require 'date'

s = Date.strptime("2010/1", "%Y/%W").to_s

unless s == '2010-01-10' ; raise 'error'; end
puts "ok"
true
