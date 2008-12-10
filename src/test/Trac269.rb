require File.expand_path('simple', File.dirname(__FILE__))
require 'uri'

nil.pause
patt = /\A(#{URI::REGEXP::PATTERN::HOST})(?::(\d+))?\z/n

str = 'localhost:3333'
a = str.scan(pattern)
nil.pause

test('localhost:3333'.scan(pattern), [["localhost", "3333"]], 'A')


report
