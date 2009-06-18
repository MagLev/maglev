# This is ok...
b = binding

# This raises an exception:
#
# error , User defined error, ' binding with arguments not supported , near line 4 of src/test/TracXXX.rb',
#          during /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
# ERROR 2318, User defined error, ' binding with arguments not supported , near line 4 of src/test/TracXXX.rb'User defined error, ' binding with arguments not supported , near line 11 of src/test/TracXXX.rb'
#
#
b2 = binding()

