require File.expand_path('simple', File.dirname(__FILE__))

# Ensure that all_symbols is now a class method (was an instance method)
# and that the _nobridge versions of the methods work.
test(:s.to_i.class, Fixnum, 'to_i')
test(Symbol.all_symbols.length > 0, true, 'all_symbols')

report
true
