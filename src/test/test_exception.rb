$HI = 0
$LO = 0
NUM = 250000 # Integer(ARGV[0] || 1)


class Lo_Exception < Exception
  def initialize(num)
    @value = num
  end
end

class Hi_Exception < Exception
  def initialize(num)
    @value = num
  end
end

def some_function(num)
  begin
    hi_function(num)
  rescue
    print "We shouldn't get here, exception is: #{$!.type}\n"
  end
end

def hi_function(num)
  begin
    lo_function(num)
  rescue Hi_Exception
    $HI = $HI + 1
  end
end

def lo_function(num)
  begin
    blowup(num)
  rescue Lo_Exception
    $LO = $LO + 1
  end
end

def blowup(num)
  if num % 2 == 0
    raise Lo_Exception.new(num)
  else
    raise Hi_Exception.new(num)
  end
end


i = 1
max = NUM+1
while i < max
  i+=1
  some_function(i+1)
end


msg_a = "Message A"
msg_b = "Message B"
e1 = Exception.new(msg_a)
m = "Expecting #{msg_a.inspect} but got #{e1.message.inspect} from e1"
raise m unless e1.message == msg_a

e2 = e1.exception
if ! e1.equal?(e2)
  raise "Expecting ! e1.equal?(e2)"
end
e3 = e1.exception(msg_b)
if  e1.equal?(e3)
  raise "Expecting e1.equal?(e3)"
end

actual = e3.message
raise "Expecting #{msg_b.inspect} but got #{actual.inspect} from e3" unless actual == msg_b




# From RubyGems
class FooException < SystemExit
  attr_accessor :exit_code
  def initialize(exit_code)
    @exit_code = exit_code
    super "Exiting RubyGems with exit_code #{exit_code}"
  end
end

begin
  raise FooException, 1
rescue FooException => ex
  raise "Fail FooException exit code: #{ex.exit_code}" unless ex.exit_code == 1
  raise "Fail FooException message" unless ex.message == "Exiting RubyGems with exit_code 1"
end

begin
  raise SystemExit, 12
rescue SystemExit => ex
  raise "Fail SystemExit 12 status: #{ex.status.inspect}" unless ex.status == 12
  raise "Fail SystemExit 12 message" unless ex.message == 'SystemExit'
end

begin
  raise SystemExit.new
rescue SystemExit => ex
  raise "Fail SystemExit default status: #{ex.status.inspect}" unless ex.status == 0
  raise "Fail SystemExit default message" unless ex.message == 'SystemExit'
end

begin
  exit(99)
rescue SystemExit => ex
  raise "Fail exit(99)" unless ex.status == 99
end

# inspect on NoMethodError was generating an error trying to coerce the
# selector (a symbol) with to_str.
# The test case passes if no exception is raised.
e = NoMethodError.new
e._init(:foo, nil, 1)
e.inspect

0
