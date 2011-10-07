# Distilled from Rack Mount / Multimap / Multiset
#
# This test case works if -MallIvsDynamic is turned on.
#
# Maglev generates a dynamic Instvar get for @rg in the instance eval
# code. BUT, @rg is a fixed inst var.  So, if you invoked the dynamically
# created method, recognize(), you'd be trying to access the fixed inst var
# with a dynmaic byte code, which yielded nil, rather than the real object.
#
#   4/    46   112    SEND sel=Symbol(oop 104219905) #__atEvalBinding_put:: env:1, 2 args, sendCache.u2=296
#               120    LOAD_LITERAL_oldGen_u1 u1=c0 flag=0 zlitIdx:36 literal:aClass:  GsProcess
#               124    PUSH_LITERAL_oldGen_u1 u1=cc flag=0 zlitIdx:38 literal:Symbol(oop 43905025) #container
#               128    PUSH_STK_s1_dynIV_u2 s1=32 u2=208 zlitIdx:39 literal:Symbol(oop 182552065) #@rg
#                                  ^^^^
#
# MagLev error was:
#  $ mruby $pbm
#  ERROR 2010 , NoMethodError: undefined method `[]' for NilClass (MessageNotUnderstood)
#
class C
  def initialize
    # Creates a fixed inst var @rg (unless -MallIvsDynamic is turned on,
    # then @rg is dynamic)
    @rg = {:foo => 22}
  end
end

c = C.new
c.instance_eval(<<-RUBY, __FILE__, __LINE__)
  def recognize()
    @rg[:foo]  # Raises undefined method `[]' for NilClass
  end
RUBY

p c.recognize()  # => 22
#################### Trac Info
# ID:         833
# Summary:    undefined method `[]' for NilClass because using Rack::Mount
# Changetime: 2011-01-12 22:48:53+00:00
###

#  Sometimes you'd get  undefined method `[]' for NilClass when using Rack::Mount.  It would go away if you passed -MallIvsDynamic. 
#  
#  To reproduce:
#  
#  {{{
#  # Distilled from Rack Mount / Multimap / Multiset
#  #
#  # This test case works if -MallIvsDynamic is turned on.
#  #
#  # Maglev generates a dynamic Instvar get for @rg in the instance eval
#  # code. BUT, @rg is a fixed inst var.  So, if you invoked the dynamically
#  # created method, recognize(), you'd be trying to access the fixed inst var
#  # with a dynmaic byte code, which yielded nil, rather than the real object.
#  #
#  #   4/    46   112    SEND sel=Symbol(oop 104219905) #__atEvalBinding_put:: env:1, 2 args, sendCache.u2=296
#  #               120    LOAD_LITERAL_oldGen_u1 u1=c0 flag=0 zlitIdx:36 literal:aClass:  GsProcess
#  #               124    PUSH_LITERAL_oldGen_u1 u1=cc flag=0 zlitIdx:38 literal:Symbol(oop 43905025) #container
#  #               128    PUSH_STK_s1_dynIV_u2 s1=32 u2=208 zlitIdx:39 literal:Symbol(oop 182552065) #@rg
#  #                                  ^^^^
#  #
#  # MagLev error was:
#  #  $ mruby $pbm
#  #  ERROR 2010 , NoMethodError: undefined method `[]' for NilClass (MessageNotUnderstood)
#  #
#  class C
#    def initialize
#      # Creates a fixed inst var @rg (unless -MallIvsDynamic is turned on,
#      # then @rg is dynamic)
#      @rg = {:foo => 22}
#    end
#  end
#  
#  c = C.new
#  c.instance_eval(<<-RUBY, __FILE__, __LINE__)
#    def recognize()
#      @rg[:foo]  # Raises undefined method `[]' for NilClass
#    end
#  RUBY
#  
#  p c.recognize()  # => 22
#  }}}
#  