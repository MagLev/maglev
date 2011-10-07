require 'test/unit'

class TC_MyTest < Test::Unit::TestCase
  def test_succeed
    assert(true, 'Assertion was true.')
  end
end
 
#################### Trac Info
# ID:         483
# Summary:    Blank output from simple test/unit failure test
# Changetime: 2010-02-09 23:00:18+00:00
###

#  Here's a simple test case, requires current .git version which includes test/unit. From running other more complex tests, it seems that MagLev is executing the test, just not printing the results.
#  
#  {{{
#  require 'test/unit'
#  
#  class TC_MyTest < Test::Unit::TestCase
#    def test_fail
#      assert(false, 'Assertion was false.')
#    end
#  end
#  }}}
#  
#  Ruby output:
#  {{{
#  $ ruby test1.rb 
#  Loaded suite test1
#  Started
#  F
#  Finished in 0.009456 seconds.
#  
#    1) Failure:
#  test_fail(TC_MyTest) [test1.rb:5]:
#  Assertion was false.
#  <false> is not true.
#  
#  1 tests, 1 assertions, 1 failures, 0 errors
#  
#  }}}
#  
#  MagLev output (not!):
#  
#  {{{
#  monty@congo:testtest $ maglev-ruby test1.rb 
#  
#  monty@congo:testtest $ 
#  
#  }}}
#  