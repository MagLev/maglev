require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

test(''.class == String, true, "''.class == String")

test(String.class == Class, true, "String.class == Class")
test(String.class.eql?(Class), true, "String.class.eql? Class")
test(String.class.equal?(Class), true, "String.class.equal? Class")

test(String.class == Fixnum.class, true, "String.class == Fixnum.class")

begin
  class ClsOne < Array 
    self.__fixed_instvars('@bb', '@cc')
    def setBb(a)
      @bb = a
    end
    def setCc(a)
      @cc = a
    end
  end
  o = ClsOne.new
  x = o.instance_variables
  test(x , [ ] , "ivs not defined" )
  o.setCc(6)
  x = o.instance_variables
  test(x , [ '@cc' ] , "cc defined" )
  o.setBb(5)
  x = o.instance_variables
  test(x , [ '@bb', '@cc'] , "plus bb defined" )
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
#require 'socket'
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
  File.open(__FILE__, "r"),        # File
  File.stat(__FILE__),             # FileStat
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
#  TCPSocket.open('localhost'),
  "a string",
  Struct.new(:foo, :bar),
  :a_symbol,
  Thread.new { sleep 1 },
  ThreadGroup.new,
  Time.now
]

p x.inspect # The test passes if there is no exception


# Regression for Trac378
result = Object.methods
test(result.kind_of?(Array), true, 'Object.methods A')
test(result.length > 20,     true, 'Object.methods B')
%w(== === =~ __id__ __send__).each do |m_name|
  test(result.include?(m_name), true, "Object.methods.include?(#{m_name})")
end

# respond_to? was having issues with bridge logic and sends of zsuper
#
class C
  def quux
    10
  end
  private :quux

  def respond_to?(*args)
    super
  end
end

c = C.new

test(c.respond_to?(:to_s),        true, "respond_to? A")
test(c.respond_to?(:to_s, true),  true, "respond_to? B")
test(c.respond_to?(:to_s, false), true, "respond_to? C")

test(c.respond_to?(:quux),        false, "respond_to? D")
test(c.respond_to?(:quux, true),  true,  "respond_to? E")
test(c.respond_to?(:quux, false), false, "respond_to? F")

args = [:to_s]
test(c.respond_to?(*args), true, "respond_to? G")

args = [:to_s, true]
test(c.respond_to?(*args), true, "respond_to? H")

args = [:to_s, false]
test(c.respond_to?(*args), true, "respond_to? I")


args = [:quux]
test(c.respond_to?(*args), false, "respond_to? J")

args = [:quux, true]
test(c.respond_to?(*args), true, "respond_to? K")

args = [:quux, false]
test(c.respond_to?(*args), false, "respond_to? L")

# There was a bug in public_methods: it didn't honor the flag.  Test the fix here.
test(c.public_methods(false), [], 'public_methods(false) A')
test(c.methods(false), [], 'methods(false) A')
def c.a_singleton_method
end
test(c.public_methods(false), ["a_singleton_method"], 'public_methods(false) B')
test(c.methods(false), ["a_singleton_method"], 'methods(false) B')

# ============== coverage for __fixed_instvars
$bz = 0
begin
  class BadA < ClsOne
     self.__fixed_instvars('@bb', '@zz')  # duplicate iv name
  end  
rescue StandardError => e
  $bz += 5
end
test( $bz , 5 , 'duplicate fixed instvar')

begin
  eval("class ClsOne; __fixed_instvars('@bb', '@cc') ; end") #illegal second opening
rescue ArgumentError => e
  $bz += 5
end
test( $bz , 10 , 'fixed instvar in second opening')

begin
  eval( "class ClsOne ; __fixed_instvars() ; end")  # zero args not allowed
rescue ArgumentError => e
  $bz += 5
end
test( $bz , 15 , 'empty fixed instvar list')


begin
  # illegal in RubySClassNode
  eval( "class ClsOne ; class << self ; __fixed_instvars('@dd'); end; end")
rescue ArgumentError => e
  $bz += 5
end
test( $bz , 20 , '__fixed_instvars illegal in sclass node')

  # illegal second call
begin
  eval("class ClsTwo < ClsOne; __fixed_instvars('@w', '@y') ; def ma; end; __fixed_instvars('@m') ; end")
rescue ArgumentError => e
  $bz += 5
end
test( $bz , 25 , 'illegal second call to __fixed_instvars')

report
true
