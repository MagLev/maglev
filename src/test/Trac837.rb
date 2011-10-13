# Not a complete test case,
#  To fully reproduce problem need to 
#    maglev-irb
#    irb > require 'rubygems'
#    irb > require 'faker'

eval "def Rational ; end; sx = self; nil.pause"
puts "---- private methods: #{self.private_methods.grep(/Rat/).inspect}"
raise "Fail" unless self.private_methods.grep(/Rat/) == ['Rational']
#################### Trac Info
# ID:         837
# Summary:    Faker can't be required
# Changetime: 2011-01-18 20:30:05+00:00
###

#  Seems the faker gem can't be required in irb. Unable to use it inside a dynamic attr setter with FactoryGirl either.
#  
#  maglev-25016 :002 > require 'faker'
#  NoMethodError: NoMethodError: undefined method `Rational' for 
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/date2.rb:82:in `method_missing'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/date2.rb:82:in `__compileClass'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/date2.rb:36
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `gem_original_require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:32:in `require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/date.rb:1
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `gem_original_require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:32:in `require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/psych/psych/deprecated.rb:1
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `gem_original_require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:32:in `require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/psych/psych.rb:9
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `gem_original_require'
#  ... 25 levels...
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:32:in `require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `gem_original_require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:30:in `require'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:32:in `require'
#  	from (irb):4090:in `__compileEval'
#  	from (irb):4099:in `__compileEval'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:111:in `eval_input'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:168:in `eval_input'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:274:in `signal_status'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:282:in `signal_status'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:156:in `eval_input'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:155:in `eval_input'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:51:in `start'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:70:in `start'
#  	from /home/jesse/.rvm/rubies/maglev-25016/lib/ruby/1.8/irb.rb:74:in `start'
#  	from /home/jesse/.rvm/rubies/maglev-25016/bin/irb:34