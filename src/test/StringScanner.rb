require File.expand_path('simple', File.dirname(__FILE__))

require 'strscan'


# Basic tests from the rdoc
s = StringScanner.new('This is an example string')
test(s.eos?, false, "s.eos")

test(s.scan(/\w+/), "This", "a")      # -> "This"
test(s.scan(/\w+/), nil, "b")      # -> nil
test(s.scan(/\s+/), ' ', 'c')      # -> " "
test(s.scan(/\s+/), nil, 'd')      # -> nil
test(s.scan(/\w+/), 'is', 'e')      # -> "is"
test(s.eos?, false, 'f')               # -> false

# From a bug in rails
s = StringScanner.new("\\A\\/posts(?:\\.(?:<format>[^\\/.?]+))?\\Z")
test(s.pos, 0, 'pos 1')

test(s.skip_until(/\(/), 10, "bug a")   #  "\\A\\/posts(?:\\.(?:<format>[^\\/.?]+))?\\Z"
                                        #              ^
test(s.pos, 10, 'pos 2')

test(s.scan(/\?:<([^>]+)>/), nil, "bug b")
test(s.pos, 10, 'pos 3')

test(s.skip_until(/\(/), 5, "bug c")    #  "\\A\\/posts(?:\\.(?:<format>[^\\/.?]+))?\\Z"
                                        #                    ^
test(s.pos, 15, 'pos 4')  # Maglev reports 25

test(s.matched, '(', 'bug c1')
test(s.pos, 15, 'pos 5')

test(s.scan(/\?:<([^>]+)>/), "?:<format>", "bug d")

test(s[1], "format", "bug e")
test(s.skip_until(/\(/), nil, "bug f")
report

true
