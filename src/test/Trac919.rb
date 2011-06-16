# MagLev raises an exception:
#
# error , a ArgumentError occurred (error 2718), cannot change protection of method in code_gen, aGsNMethod,
#              during /Users/pmclain/tmp/pbm.rb
# ERROR 2718 , a ArgumentError occurred (error 2718), cannot change protection of method in code_gen, aGsNMethod (ArgumentError)
#
class OrderedOptions < Hash
  alias_method :_get, :[]
  protected :_get
end
true
