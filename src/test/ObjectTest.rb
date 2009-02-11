require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

test(''.class == String, true, "''.class == String")

test(String.class == Class, true, "String.class == Class")
test(String.class.eql?(Class), true, "String.class.eql? Class")
test(String.class.equal?(Class), true, "String.class.equal? Class")

test(String.class == Fixnum.class, true, "String.class == Fixnum.class")

begin
  class ClsOne < Hash
    def setBb(a)
      @bb = a
    end
    def setCc(a)
      @cc = a
    end
  end
  o = ClsOne.new
  x = o.instance_variables
  test(x , [ '@bb', '@cc' ] , "instance_variables fixed")
  class ClsOne
    def setDd(a)
      @dd = a
    end
    def to_a
      res = [ @bb , @cc , @dd ]
    end
  end
  x = o.instance_variables
  test(x , [ '@bb', '@cc' ] , "instance_variables fixed 2")
  o.setDd(20)
  x = o.instance_variables
  test(x , [ '@bb', '@cc' , '@dd' ] , "ivs3")
  o.setBb(9) ;   o.setCc(15)
  x = o.to_a
  test(x , [ 9, 15, 20 ] , "ivs4")
  p = ClsOne.new
  p.setCc(8) ;
  x = p.to_a
  test(x , [ nil, 8, nil ] , "ivs5")
  test(o.instance_variable_get( :'@bb' )  , 9 , "ivs6")
  test(o.instance_variable_get( :'@dd' )  , 20 , "ivs6")
  test(p.instance_variable_get( :'@dd' ), nil, "ivs7")
  o.instance_variable_set( :'@dd' , 30)
  p.instance_variable_set( :'@dd' , 35)
  test(o.to_a, [ 9, 15, 30 ], "ivs7")
  test(p.to_a, [ nil, 8, 35 ], "ivs8")
end

# Test for instance_eval

class Klass
  def initialize
    @secret = 99
  end
end

k = Klass.new
test(k.instance_eval { @secret }, 99, 'instance_eval { @secret }')
test(k.instance_eval("@secret"), 99, 'instance_eval("@secret")')

def test_eval_with_tilde
  a = $~
  o = Klass.new
  r = o.instance_eval( ' /cd/ =~ "abcded" ' )
  unless r == 2 ; raise 'Err' end;
  b = $~
  unless a == nil ; raise 'Err' end;
  unless b.class.equal?(MatchData); raise 'Err' end;
end
test_eval_with_tilde()


# Test methods: Just a smoke test to ensure the method is recognized
# and does something approximately correct.
class Methods
  def a_method
  end
end

m = Methods.new.methods
some_methods = ["a_method", "methods"].sort
test((m & some_methods).sort, some_methods, "Object#methods 1")


# Ruby inspect takes no parameters, but the implementation in array, and
# other containers adds a touchedSet parameter to dectect infinite loop.
# Object defines inspect(touchedSet=nil), but the bridge logic doesn't find
# it and if there is, e.g., a Regexp in an array, then inspect fails with
# wrong number of parameters.
#
#    [ /xyz/ ].inspect => Error, 'too many arguments'
#
# Since regexp is not a container, we just ignore the touched set and
# call the normal inspect.
#
# This test creates an array with an instance of all the concrete classes
# and then inspects it to make sure inspect is well defined on all Classes.
#
require 'socket'
x = [ [1,2,3],                    # Array
  999898989898989898989898989898, # Bignum
  binding,
  true,
  false,
  Class.new,
  Dir.new("/tmp"),
  Errno::EBADF,
  Exception.new,
  STDERR,                          # IO
  File.open("/tmp/foo", "w+"),     # File
  File.stat("/tmp"),               # FileStat
  10,                              # Fixnum
  10.4,                            # Float
  { "a" => 1},                     # Hash
  IdentitySet.new,
  # Integer is abstract
  /xyz/.match("foo"),              # MatchData
  Mutex.new,
  nil,
  Object.new,
  Proc.new { "a proc"},
  Random.new,
  (0..4),
  /xyz/,
#  TCPSocket.open('localhost', 7777),
  "a string",
  Struct.new(:foo, :bar),
  :a_symbol,
  Thread.new { sleep 1 },
  ThreadGroup.new,
  Time.now
]

p x.inspect # The test passes if there is no exception

report
true
