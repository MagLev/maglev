#
# Tests the following:
#   simple puts
#   compound puts
#   puts with variable

# Class definition
class PutsTest
    # Expected value: Hello World!
    def simplePuts
        puts "Hello World!"
    end

    # Expected value: Hello World!
    def compoundPuts
        puts "Hello" + " World" + "!"
    end

    # Expected value: Hello <var>
    def simpleStrVarPuts (var)
        puts "Hello " + var
    end

    # Expected value: Hello <var>
    def simpleIntVarPuts (var)
        puts "Hello " + var.to_s
    end

    # Expected value: Hello <var> and <var>
    def compoundVarPuts (var1, var2)
        puts "Hello " + var1 + " and " + var2
    end
end


###
# Call test methods
###

puts ""
puts "****************************"
puts "Beginning method invocations"
puts "****************************"
puts ""

# expectvalue 'Hello World!'
puts " "
puts "Calling PutsTest.new.simplePuts"
PutsTest.new.simplePuts
puts "Completed PutsTest.new.simplePuts"

# expectvalue 'Hello World!'
puts " "
puts "Calling PutsTest.new.compoundPuts"
PutsTest.new.compoundPuts
puts "Completed PutsTest.new.compoundPuts"

# expectvalue 'Hello there' 
puts ""
puts "Calling PutsTest.new.simpleStrVarPuts('there')"
PutsTest.new.simpleStrVarPuts('there')
puts "Completed PutsTest.new.simpleStrVarPuts('there')"

# expectvalue 'Hello 345'
puts ""
puts "Skipping PutsTest.new.simpleIntVarPuts(345)"
#PutsTest.new.simpleIntVarPuts(345)
#puts "Completed PutsTest.new.simpleIntVarPuts(345)"

# expectvalue 'Hello Fred and Wilma'
puts ""
puts "Calling PutsTest.new.compoundVarPuts('Fred', 'Wilma')"
PutsTest.new.compoundVarPuts('Fred', 'Wilma')
puts "Completed PutsTest.new.compoundVarPuts('Fred', 'Wilma')"

puts ""
puts "****************************"
puts "Test complete"
puts "****************************"
puts ""