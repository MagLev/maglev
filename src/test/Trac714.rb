# Maglev has yet another protection problem.
#
#   respond_to?: false
#   #<RuntimeError: Fail>
#   /Users/pmclain/GemStone/dev/pbm.rb:17:in `raise'
#   /Users/pmclain/GemStone/dev/pbm.rb:17:in `raise'
#   /Users/pmclain/GemStone/dev/pbm.rb:17:in `load_file'
#   /Users/pmclain/GemStone/dev/pbm.rb:28:in `m'
#   /Users/pmclain/GemStone/dev/pbm.rb:32
#   ERROR 2023, Error, 'Fail' (RuntimeError)

module Base
  protected

  def load_file
    raise 'Fail' unless respond_to?(:load_yml)
    714
  end

  def load_yml
    10
  end
end

class C
  include Base
  def m
    load_file
  end
end

x = C.new.m
unless x == 714 ; raise 'error'; end
true


require File.expand_path('simple', File.dirname(__FILE__))

class C
  def m1
  end

  protected
  def m2
  end

  private
  def m3
  end
end

c = C.new

test(c.respond_to?(:m1),        true, "public method")
test(c.respond_to?(:m1, true),  true, "public method true")
test(c.respond_to?(:m1, false), true, "public method false")

test(c.respond_to?(:m2),        true, "protected method")
test(c.respond_to?(:m2, true),  true, "protected method true")
test(c.respond_to?(:m2, false), true, "protected method false")

test(c.respond_to?(:m3),        false, "private method")
test(c.respond_to?(:m3, true),  true,  "private method true")
test(c.respond_to?(:m3, false), false, "private method false")

test(c.respond_to?(:m4),        false, "undefined method")
test(c.respond_to?(:m4, true),  false, "undefined method true")
test(c.respond_to?(:m4, false), false, "undefined method false")

report
true
