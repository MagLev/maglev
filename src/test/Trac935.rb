# This is distilled from
#   $MAGLEV_HOME/lib/maglev/gems/1.8/gems/activesupport-3.1.0.rc4/lib/active_support/testing/assertions.rb
#
# MRI:
#   $ ruby $pbm
#   14
#
# MagLev:
#   $ maglev-ruby $pbm
#   error , Proc#binding not supported,
#                during /Users/pmclain/tmp/pbm.rb
#   ERROR 2702 , Proc#binding not supported (NotImplementedError)
#   topaz 1>

class C
  def assert_difference(expression, &block)
    eval(expression, block.binding)
  end

  def do_it
    a = 10
    assert_difference("p a + 4") do
      p 'hi'
    end
  end
end

C.new.do_it
