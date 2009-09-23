#
# Tests the following:
#   simple puts
#   compound puts
#   puts with variable

# Class definition
class PutsTest
    # Expected value: nil
    def simplePuts
        puts "Hello World!"
    end

    # Expected value: nil
    def compoundPuts
        puts "Hello" + " World" + "!"
    end

    # Expected value: nil
    def simpleStrVarPuts (var)
        puts "Hello " + var
    end

    # Expected value: nil
    def simpleIntVarPuts (var)
        puts "Hello " + var.to_s
    end

    # Expected value: nil
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

# expectvalue nil
ret = PutsTest.new.simplePuts
raise "ERROR" unless ret == nil

# expectvalue nil
PutsTest.new.compoundPuts
raise "ERROR" unless ret == nil

# expectvalue nil
PutsTest.new.simpleStrVarPuts('there')
raise "ERROR" unless ret == nil

# expectvalue nil
PutsTest.new.simpleIntVarPuts(345)
raise "ERROR" unless ret == nil

# expectvalue nil
PutsTest.new.compoundVarPuts('Fred', 'Wilma')
raise "ERROR" unless ret == nil
