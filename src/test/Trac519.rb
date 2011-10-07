# Test days until Christmas 2009. Should report negative days afterwards.

# The original test case: Passes if there is no exception
require 'date'
puts "#{Date.new(2009,12,25) - Date.today} days until Christmas, 2009"

# Other test cases

class D
  class << self; alias_method :new!, :new end

  # NOTE: four parameters gets us out of the default selector set.  There
  # is an "extra" selector for 'civil'
  def self.civil(y=-4712, m=1, d=1, sg=:italy)
  end

  # The bug is that only the standard bridge methods are getting aliased.
  # Since civil has four parameters, the selector civil:::: is NOT getting
  # aliased, so we can't find the method.
  class << self; alias_method :new, :civil end
end

D.new(4712, 1, 1, :x)
D.new

# The root problem for 519, is also present in a few other places.  The
# following test cases check for those as well.

# remove method: Remove method has the bug, but it is not visible to ruby.
# After the call to remove_method, there will still be a selector foo:::::,
# so it leads to memory leak.  Can't really test that here, but we ensure
# the bug doesn't crop up later...
class D1
  def foo(a=1,b=2,c=3,d=4,e=5)
    10
  end
end

raise 'foo not defined' unless D1.new.foo == 10

class D1
  remove_method(:foo)
end

begin
  D1.new.foo
  fail "Expecting foo to be removed"
rescue NoMethodError => nme
  # Ok!!
end

# To really test this, we should do something like:
#
#    selectors := D1 rubySelectorsWithPrefix: 'foo' env: 1.
#    selectors size ~~ 0 ifTrue:[ "fail" ] .

# Unbound Methods: This will call RubyUnboundMeth>>_nonBridgeGsMethod:,
# which is also affected by the change.
class D2
  def bar
    puts "Hi from bar"
  end
  def many_args(a=1, b=2, c=3, d=4, e=5)
    10
  end
end

m2 = D2.new.method(:many_args)
raise 'Bad return' unless m2.call == 10

# Module Methods: expected to be broken, but wasn't...
#
# Module>>addModuleMethod was not updated to the new way of iterating over
# the bridge methods (i.e., use Behavior>>rubySelectorsWithPrefix:env:).
# The current way works, because the bridge methods use SEND_CURRENT, and
# so when they are copied over to the new meth dict, they already have all
# the info they need to call correctly.  In the case below,
# a_module_method:::* will be called, but the SEND_CURRENT already has the
# correct superOop in it, so it just works.  This test case is left, just
# in case things change in the future.
module M
  def a_module_method(a=1, b=2, c=3, d=4, e=5)
    27
  end
  module_function :a_module_method
end
raise "Fail module method" unless M.a_module_method(22,33,44,55,66) == 27
raise "Fail module method 2" unless M.a_module_method(12,13,14,15) == 27

# Setting protection on methods
#
# This case "works" for the same reason as the module methods version
# "works".  We go through one of the standard bridge methods, and the
# protection is detected there, even though it is not set on the others
class D3
  def one_arg(a=1)
    puts "one_arg(#{a})"
    a
  end

  def many_args(a=1, b=2, c=3, d=4, e=5)
    44
  end
  private :one_arg, :many_args
end

d3 = D3.new
begin
  d3.one_arg
  fail "Expecting NoMethodError at A"
rescue NoMethodError
  # OK
end

begin
  d3.one_arg(3)
  fail "Expecting NoMethodError at B"
rescue NoMethodError
  # OK
end

begin
  d3.many_args
  fail "Expecting NoMethodError at C"
rescue NoMethodError
  # OK
end

begin
  d3.many_args(9,8,7,6,5)
  fail "Expecting NoMethodError at D"
rescue NoMethodError
  # OK
end

begin
  d3.many_args(9,8,7,6)
  fail "Expecting NoMethodError at E"
rescue NoMethodError
  # OK
end

begin
  d3.many_args(9,8,7)
  fail "Expecting NoMethodError at F"
rescue NoMethodError
  # OK
end

begin
  d3.many_args(9,8)
  fail "Expecting NoMethodError at G"
rescue NoMethodError
  # OK
end

raise "Fail with send A" unless  d3.send(:many_args) == 44
raise "Fail with send A" unless  d3.send(:many_args, 55, 66) == 44

#################### Trac Info
# ID:         519
# Summary:    Date.new unimplemented
# Changetime: 2009-08-08 17:22:09+00:00
###

#  Playing around with dates as they are reportedly very slow in MRI, I discovered:
#  
#  {{{
#  require 'date'
#  puts "#{Date.new(2009,12,25) - Date.today} days until Christmas, 2009"
#  NoMethodError: Undefined method `new' for Date  
#  	from /congo1/users/monty/MagLev/MagLev-21605.Linux/bin/maglev-irb:24: in 'Object >> _compileFile'
#  }}}
#  
#  I'll create and checkin a test file for this ticket
#  