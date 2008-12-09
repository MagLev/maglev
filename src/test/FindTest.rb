require File.expand_path('simple', File.dirname(__FILE__))
require 'find'

count = 0
Find.find('.') { |path| count += 1 }

test(count > 0, true, 'Find smoke test')

report

