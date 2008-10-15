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

0
