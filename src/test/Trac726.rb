# Unit tests using the assert_raise assertion stop all subsequent tests
# from running.  If you comment out the assert_raise line in test_b, then
# you'll see all three tests run in alphabetical order: test_a, test_b,
# test_c.  If you leave the assert_raise line active, then test_c does not
# run.

require 'test/unit'

$flag = false

$aa = []

class MyTests < Test::Unit::TestCase
  def test_a
    puts "---- in test_a"
    assert(true)
    $aa << 10
  end

  def test_b
    puts "---- in test_b"
    assert_raise(RuntimeError) { raise 'xxx' }
    assert(true)
    $aa << 20
  end

  def test_c
    puts "---- in test_c"
    assert(true)
    $flag = true
    $aa << 30
  end
end
r = Test::Unit::AutoRunner.new(false) 
r.run
unless (ax = $aa) == [ 10, 20, 30 ] ; raise 'error' ; end
true
#################### Trac Info
# ID:         726
# Summary:    Test::Unit assert_raise assertion is broken
# Changetime: 2010-04-29 17:25:13+00:00
###

#  The assert_raise assertion is broken.  If there is a test that uses assert_raise, and the block raises an exception, then no more tests are run for that test class.
#  
#  
#  {{{
#  # Unit tests using the assert_raise assertion stop all subsequent tests
#  # from running.  If you comment out the assert_raise line in test_b, then
#  # you'll see all three tests run in alphabetical order: test_a, test_b,
#  # test_c.  If you leave the assert_raise line active, then test_c does not
#  # run.
#  
#  require 'test/unit'
#  
#  $flag = false
#  
#  class MyTests < Test::Unit::TestCase
#    def test_a
#      puts "---- in test_a"
#      assert(true)
#    end
#  
#    def test_b
#      puts "---- in test_b"
#      assert_raise(RuntimeError) { raise 'xxx' }
#      assert(true)
#    end
#  
#    def test_c
#      puts "---- in test_c"
#      assert(true)
#      $flag = true
#    end
#  end
#  
#  }}}
#  
#  MRI runs 3 tests and 4 assertions:
#  
#  {{{
#  $ ruby src/test/TracXXX.rb
#  Loaded suite src/test/TracXXX
#  Started
#  ---- in test_a
#  .---- in test_b
#  .---- in test_c
#  .
#  Finished in 0.000563 seconds.
#  
#  3 tests, 4 assertions, 0 failures, 0 errors
#  
#  }}}
#  
#  Maglev only runs 1 test and 2 assertions:
#  
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb
#  Loaded suite src/test/TracXXX
#  Started
#  ---- in test_a
#  .---- in test_b
#  
#  Finished in 0.001561 seconds.
#  
#  1 tests, 2 assertions, 0 failures, 0 errors
#  during at_exit handler 1: error , SystemExit
#  }}}
#  
#  
#  