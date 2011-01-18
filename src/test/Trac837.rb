# Not a complete test case,
#  To fully reproduce problem need to 
#    maglev-irb
#    irb > require 'rubygems'
#    irb > require 'faker'

eval "def Rational ; end; sx = self; nil.pause"
puts "---- private methods: #{self.private_methods.grep(/Rat/).inspect}"
raise "Fail" unless self.private_methods.grep(/Rat/) == ['Rational']
