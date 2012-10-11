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
