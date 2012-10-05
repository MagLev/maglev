# https://github.com/MagLev/maglev/issues/143
#
# This code returns an enumerator
#
# "abc".chars # => #<Enumerable::StringCharEnumerator:0x299bd901 @ofs=0 @obj="abc" @enum_selector=:chars @extra_args=[]>
#
# Once you require "backports" (tested with versions 2.6.3 and 2.3.0), this happens
#
# "abc".chars
# ArgumentError: too few arguments
#         from /home/tim/Devel/vmware/maglev/src/kernel/bootstrap/String.rb:216:in `chars'
#         from (irb):3:in `__compileEval'
#         from (irb):5:in `__compileEval'
#         from /home/tim/Devel/vmware/maglev/src/kernel/bootstrap/Kernel.rb:402:in `eval'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:159:in `eval_input'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:162:in `eval_input'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:280:in `signal_status'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:281:in `signal_status'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:156:in `eval_input'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:155:in `eval_input'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:71:in `start'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:70:in `start'
#         from /home/tim/.rbenv/versions/maglev/lib/ruby/1.8/irb.rb:73:in `start'
#         from /home/tim/.rbenv/versions/maglev/bin/maglev-irb:30
#
# It seems to be in the C code for argument processing, because the Smalltalk stack doesn't tell me much
#
# ==> 24 Array >> atOrMissingArgErr:              (envId 0) @11 line 13
# 25 Enumerator # initialize_without_optional_block#0*& (envId 1b) @4 line 1
# 26 Enumerator # initialize#0*&              (envId 1) @6 line 2
# 27 Enumerator # initialize#2__              (envId 1b) @2 line 1
# 28 Metaclass3 # new#2__                     (envId 1) @4 line 3
# 29 String # chars#0_&                       (envId 1) @4 line 4
# 30 String # chars#0__                       (envId 1b) @2 line 1
# 31 Module # __compileClass                  (envId 1) @2 line 9
#
# atOrMissingArgErr: receives the argument index from somewhere in the VM, and throws an exception. Frame 26 is in backports lib/backports/1.9.1/enumerator.rb

class Enumerable::Enumerator
  def initialize_with_optional_block(*arg, &block)
    $alias_was_run = true
    return initialize_without_optional_block(*arg, &nil)
  end
  alias_method :initialize_without_optional_block, :initialize
  alias_method :initialize, :initialize_with_optional_block
end

"a".chars
raise unless $alias_was_run
$alias_was_run = false

Enumerable::Enumerator.new([])
raise unless $alias_was_run
$alias_was_run = false

Enumerable::Enumerator.new([], :each)
raise unless $alias_was_run
$alias_was_run = false

Enumerable::Enumerator.new([], :each, :extra1)
raise unless $alias_was_run
$alias_was_run = false

Enumerable::Enumerator.new([], :each, :extra1, :extra2)
raise unless $alias_was_run
$alias_was_run = false

# Test passes if no exception is thrown
