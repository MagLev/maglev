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
