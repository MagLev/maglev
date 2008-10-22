require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

d = Dir.new('/tmp')

# The following tests are order dependent (i.e., they depend on the
# internal state of the Dir object), so don't change the order.
test(d.read,     '.', 'read A')
test(d.read,    '..', 'read B')

# TODO: ruby fails on this one (returns 1 rather than 2): why?!
#       if I manually do two reads and a pos in irb, it shows 2...
test(d.pos,        2, 'pos A')

test(d.pos = 0,    0, 'pos= A')
test(d.tell,       0, 'tell A')

# Ensure enumerable is included properly
test(d.select { |e| e == "." || e == ".."}, [".", ".."], 'Enumerable: select')


report
