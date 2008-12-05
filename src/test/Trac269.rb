require File.expand_path('simple', File.dirname(__FILE__))
require 'URI'

pattern = /\A(#{URI::REGEXP::PATTERN::HOST})(?::(\d+))?\z/n

test('localhost:3333'.scan(pattern), [["localhost", "3333"]], 'A')


report
