eval "require 'rational'"
puts "---- private methods: #{self.private_methods.grep(/Rat/).inspect}"
raise "Fail" unless self.private_methods.grep(/Rat/) == ['Rational']
