# Test the simple undefined constant case
$flag = false
$ensure = false

begin
  class SomeConstantThatIsUndefined < StandardError
  end
  raise SomeConstantThatIsUndefined.new
rescue ScriptError
  raise "should not pass through here"
rescue SomeConstantThatIsUndefined
  $flag = true
else
  raise "should not execute ELSE"
ensure
  $ensure = true
end

raise "raise got confused" unless $flag
raise "raise got confused" unless $ensure

# Test the undefined splat case
$flag = false
$ensure = false

begin
  a = []
  class SomeOtherConst < StandardError
  end
  a << SomeOtherConst << ScriptError
  raise SomeOtherConst.new
rescue ScriptError
  raise "should not pass through here"
rescue *a
  $flag = true
else
  raise "should not execute ELSE"
ensure
  $ensure = true
end

raise "raise got confused by splat rescue" unless $flag
raise "raise got confused by splat rescue" unless $ensure

# Test lazy eval of rescue-clauses
$flag = false
$ensure = false
$first = false
$before = false
$after = false

begin
  class ThirdTest < StandardError
  end
  raise ThirdTest.new
rescue ScriptError, $first = true
  raise "should not pass through here"
rescue $before = true, ThirdTest, $after = true
  $flag = true
else
  raise "should not execute ELSE"
ensure
  $ensure = true
end

raise "raise was not lazy" unless $first
raise "raise was not lazy" unless $before
raise "raise was not lazy" if $after
raise "raise was not lazy" unless $flag
raise "raise was not lazy" unless $ensure

# Test eager eval of splat rescue-clauses with more stuff after
$flag = false
$ensure = false
$first = false
$before = false
# $after = false # not supported in parser

begin
  fourth = []
  class FourthTest < StandardError
  end
  fourth << FourthTest
  raise FourthTest.new
rescue ScriptError, $first = true
  raise "should not pass through here"
rescue $before = true, *fourth #, $after = true # not supported in parser
  $flag = true
else
  raise "should not execute ELSE"
ensure
  $ensure = true
end

raise "raise was not lazy with splat rescue" unless $first
raise "raise was not unlazy with splat rescue" unless $before
# raise "raise was not unlazy with splat rescue" unless $after # not supported in parser
raise "raise was not lazy with splat rescue" unless $flag
raise "raise was not lazy with splat rescue" unless $ensure

# Test overridden ancestry
$flag = false

class A
  def ===(o)
    true
  end
end

begin
  raise StandardError
rescue A.new
  $flag = true
end

raise "rescue does not respect overridden #===" unless $flag

# Strange inner outer stuff
require 'rubygems'
require 'timeout'
begin
  begin
    begin
      begin
        raise Gem::LoadError.new "sth"
      rescue puts("inner thing"), StandardError, Timeout::Error
        raise "falsely rescued"
      end
    rescue puts("inner thing 2"), StandardError
      raise "falsy rescued"
    end
  rescue puts("right thing"), Timeout::Error, Gem::LoadError, NoMethodError
    # nil.pause
    p "rescued"
  end
rescue puts("outer thing"), ScriptError
  raise "script error??"
end

# Inline return
def foo
  raise StandardError rescue return
end

# Lookup problems
class Guah
  class Exception < ::StandardError; end
  class MyException < ::Exception; end

  def foo
    begin
      raise MyException
    rescue MyException
      # This will be replaced with the following:
      # rescue ::Exception        <--- this used to be just Exception,
      #                                which would be the wrong thing in this scope
      #   if MyException === $!
      #     ...
      #   else
      #     $!.__reraise
      #   end
      # end
      puts "stderror"
    end
  end
end
Guah.new.foo
