require File.expand_path('simple', File.dirname(__FILE__))
require 'uri'

patt = /\A(#{URI::REGEXP::PATTERN::HOST})(?::(\d+))?\z/n

# str = 'localhost:3333'
# a = str.scan(patt)
# nil.pause

test('localhost:3333'.scan(patt), [["localhost", "3333"]], 'A')


report
