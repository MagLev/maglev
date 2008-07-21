#
# Tests the following:
#    Add
#

# Class definition for expression tests

class ExprTest
    # Expected value: 7
    def addIntSimple
        puts 3+4
    end

    # Expected value: 12
    def addIntCompound
        puts 3+4+5
    end

    # Expected value: -2
    def subtractIntSimple
        puts 3-5
    end

    # Expected value: 5
    def subtractIntCompound
        puts 100-90-5
    end

    # Expected value: 150
    def multiplyIntSimple
        puts 50*3
    end

    # Expected value: 150
    def multiplyIntCompound
        puts 25*3*2
    end

    # Expected value: 3
    def divideIntSimple
        puts 300/100
    end

    # Expected value: 111
    def divideIntCompound
        puts 999/3/3
    end

    # Expected value: 7.4
    def addFloatSimple
        puts sprintf("%0.1f", 3.1+4.3)
    end

    # Expected value: 12.8
    def addFloatCompound
        puts sprintf("%0.1f", 3.1+4.3+5.4)
    end

    # Expected value: 5.4
    def subtractFloatSimple
        puts sprintf("%0.1f", 70.5-65.1)
    end

    # Expected value: 5.14
    def subtractFloatCompound
        puts sprintf("%0.2f", 100.54-90.1-5.3)
    end

    # Expected value: 39.06
    def multiplyFloatSimple
        puts sprintf("%2.2f", 12.6*3.1)
    end

    # Expected value: 150
    def multiplyFloatCompound
        puts sprintf("%3.2f", 25.25*3.7*2.4)
    end

    # Expected value: 3
    def divideFloatSimple
        puts sprintf("%2.13f", 255.64/3.6)
    end

    # Expected value: 111
    def divideFloatCompound
        puts sprintf("%2.14f", 3.2/2.3/0.5)
    end

    # Expected value: 2
    def modulusInt
        puts 8%3
    end

    # Expected value: 3.2
    def modulusFloat
        puts sprintf("%2.1f", 25.4%3.7)
    end

    # Expected value: 1000
    def powerInt
        puts 10**3
    end

    # Expected value: 54798.1281
    def powerFloat
        puts sprintf("%5.4f", 15.3**4)
    end

    # Expected value: 12
    def rightShift
        puts 100>>3
    end

    # Expected value: 800
    def leftShift
        puts 100<<3
    end

    # Expected value: reverse of a and b
    def parallel(a, b)
        a, b = b, a
        puts "#{a}#{b}"
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

# expectvalue 7
ExprTest.new.addIntSimple()

# expectvalue 12
ExprTest.new.addIntCompound()

# expectvalue -2
ExprTest.new.subtractIntSimple()

# expectvalue 5
ExprTest.new.subtractIntCompound()

# expectvalue 150
ExprTest.new.multiplyIntSimple()

# expectvalue 100
ExprTest.new.multiplyIntCompound()

# expectvalue 3
ExprTest.new.divideIntSimple()

# expectvalue 111
ExprTest.new.divideIntCompound()

# expectvalue 7.4
ExprTest.new.addFloatSimple()

# expectvalue 12.8
ExprTest.new.addFloatCompound()

# expectvalue 5.4
ExprTest.new.subtractFloatSimple()

# expectvalue 5.14
ExprTest.new.subtractFloatCompound()

# expectvalue 39.06
ExprTest.new.multiplyFloatSimple()

# expectvalue 224.22
ExprTest.new.multiplyFloatCompound()

# expectvalue 71.0111111111111
ExprTest.new.divideFloatSimple()

# expectvalue 2.78260869565217
ExprTest.new.divideFloatCompound()

# expectvalue 2
ExprTest.new.modulusInt()

# expectvalue 3.2
ExprTest.new.modulusFloat()

# expectvalue 1000
ExprTest.new.powerInt()

# expectvalue 54798.1281
ExprTest.new.powerFloat()

# expectvalue 12
ExprTest.new.rightShift()

# expectvalue 800
ExprTest.new.leftShift()

# expectvalue
ExprTest.new.parallel("aaa", "bbb")
