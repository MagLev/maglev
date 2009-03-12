# This file holds test cases that used to be in BrokenRegressions.rb, but
# have subsequently been fixed.  This file is run by vmunit.conf, so that
# we can ensure we don't regress on these ad-hoc cases.
require File.expand_path('simple', File.dirname(__FILE__))

# The Rubinius Struct.rb does this
class Foo
  class << self
    alias_method :my_new, :new
  end
end

oa = Foo.new
unless oa.class.equal?(Foo) ; raise 'Error' ; end
ob = Foo.my_new
unless ob.class.equal?(Foo) ; raise 'Error' ; end

##################################################

begin
  ary = [1,2,3]
  ary["cat"]
rescue TypeError
  # Nothing
rescue Exception => e
  puts "non TypeError unacceptable...#{e}"
end



#### From the Mspec framework

obj = Object.new
def obj.start
  @width = 12  # Blows up: No method found for the selector #'indexOfIdentical:'
end
def obj.width
  @width
end
obj.start

test(obj.width, 12, 'dynamic instvar created from singleton')

### From Ramaze
# There was a bug in which alias_method would only accept symbols.  Now
# it should also accept strings.
class String
  def escape which = :html
  end
  alias_method 'esc', 'escape'
end



####################################################
# Distilled from the maglev-discussion group 2009-03-12
#
# These are several of the test cases Markus had a problem with.  They seem
# to work in maglev-ruby, but many fail in maglev-irb.
#
####################################################
p = Proc.new { |a,b| [a,b] }
test(p.call(1,2), [1,2], "Fail at A")

def l(&b); lambda &b; end
p = l { |a,b| [b,a] }
test(p.call(1,2), [2,1],"Fail at B")


p = lambda { :ho }; class << p; define_method(:call) { :ho }; end
test(p.call, :ho, "Fail at C")

# ...or, without having to duplicate the block:

p2 = lambda { :ah_ha }; $proc_kludge = p2; class << p2; define_method(:call,$proc_kludge); end
test(p2.call, :ah_ha, "Fail at D")

# But alas, it was not to be, as I discovered as soon as I tried it with
# arguments:

p2 = lambda { |a,b| [a,b] }; $proc_kludge = p2; class << p2; define_method(:call,$proc_kludge); end
test(p2.call(1,2), [1,2], "Fail at E")

# And with arguments, the verbose method doesn't work either.

p = lambda { |a,b| }; class << p; define_method(:call) { |a,b| [a,b] }; end
test( p.call(1,2), [1,2], "Fail at F")

# *sigh*

# So it looks like we just have to face the fact that we aren't getting
# anything callable back.  But wait, what if we do it in a single
# expression?

test(l { |a,b| [a,b] }.call(1,2), [1,2], 'Fail at G')

#        1
#        2
#        => nil

# Interesting.  So is it putting it in a variable that breaks things?

test((l { |a,b| [a,b] }).call(1,2), [1,2], 'Fail at H' )

#        1
#        2
#        => nil

# Ok..this isn't really the original...
(p = l { |a,b| [a,b] });
test(p.call(1,2), [1,2], 'Fail at I')
#        1
#        2
#        => nil

# No, apparently not.  Yet if we spread it out over several lines:

p = l { |a,b| [a,b]}
#        => #<Proc>
test(p.call(1,2), [1,2], 'Fail at J')
#        Undefined method `call' for Object

#        .. END
#        {:commitResult=>:success}
#        *> p
#        => #<Object:0x05985001>




report
