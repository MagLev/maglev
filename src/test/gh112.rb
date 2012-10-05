# https://github.com/MagLev/maglev/issues/114
#
# MRI 1.8.7-p352
#
#   >> p = Proc.new { |s, *args| p s, args }
#   => #<Proc:0x00007f2fa3e98ef8@(irb):6>
#   >> p.call(:foo)
#   :foo
#   []
#
# MagLev
#
#   >> p = lambda { |s, *args| p s, args }
#   => #<Proc>
#
#   >> p.call(:foo)
#   ArgumentError: too few args
#       from (irb):10:in `__compileEval'
#       from (irb):12:in `__compileEval'
#       from /home/jesse/projects/maglev/src/kernel/bootstrap/Kernel.rb:394:in `eval'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:159:in `eval_input'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:162:in `eval_input'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:280:in `signal_status'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:281:in `signal_status'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:156:in `eval_input'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:155:in `eval_input'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:71:in `start'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:70:in `start'
#       from /home/jesse/projects/maglev/lib/ruby/1.8/irb.rb:73:in `start'
#       from /home/jesse/projects/maglev/bin/maglev-irb:30

# Proc works
p = Proc.new { |s, *args| p s, args }
p.call(:foo)

# Lambda fails
l = lambda { |s, *args| p s, args }
l.call(:foo)

# Test passes if no exception thrown
