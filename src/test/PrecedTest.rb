#
# Tests the following:
#    precedence

# Class definition for precedence tests

class PrecedTestDef
    # Expected value: 3775
    def refPower
        array = Array.new(1)
        array[0]=15
        puts array[0]**3
    end

    # Expected value: 490
    def powerPlus
        puts 22**2+6
    end

    # Expected value: 478
    def powerMinus
        puts 22**2-6
    end

    # Expected value: 210
    def plusMultiply
        puts 10+20*10
    end

    # Expected value: -190
    def minusMultiply
        puts 10-20*10
    end

    # Expected value: 70
    def plusDivide
        puts 10+20/4
    end

    # Expected value: 5
    def minusDivide
        puts 10-20/4
    end

    # Expected value: 49
    def plusModulus
        puts 49+15%3
    end

    # Expected value: 48
    def minusModulus
        puts 49-15%2
    end

    # Expected value: 12
    def plusRightShift
        puts 100>>2+1
    end

    # Expected value: 800
    def plusLeftShift
        puts 100<<2+1
    end

    # Expected value: 12
    def minusRightShift
        puts 100>>4-1
    end

    # Expected value: 800
    def minusLeftShift
        puts 100<<4-1
    end

    # Expected value: false
    def lessLogicalAnd
        if 3<4&&4<3
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true
    def lessLogicalOr
        if 3<4||4<3
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: false
    def greaterLogicalAnd
        if 3>4&&4>3
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true
    def greaterLogicalOr
        if 3>4||4>3
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: false
    def equalsLogicalAnd
        if 3==3&&3==4
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true
    def equalsLogicalOr
        if 3==3||4==4
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: false
    def notEqualsLogicalAnd
        if !3==3&&!4==4
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true
    def notEqualsLogicalOr
        if !3==3||!4==4
            puts "false"
        else
            puts "true"
        end
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

# expectvalue 3375
PrecedTestDef.new.refPower()

# expectvalue 490
PrecedTestDef.new.powerPlus()

# expectvalue 478
PrecedTestDef.new.powerMinus()

# expectvalue 210
PrecedTestDef.new.plusMultiply()

# expectvalue -190
PrecedTestDef.new.minusMultiply()

# expectvalue 15
PrecedTestDef.new.plusDivide()

# expectvalue 5
PrecedTestDef.new.minusDivide()

# expectvalue 49
PrecedTestDef.new.plusModulus()

# expectvalue 48
PrecedTestDef.new.minusModulus()

# expectvalue 12
PrecedTestDef.new.plusRightShift()

# expectvalue 800
PrecedTestDef.new.plusLeftShift()

# expectvalue 12
PrecedTestDef.new.minusRightShift()

# expectvalue 800
PrecedTestDef.new.minusLeftShift()

# expectvalue false
PrecedTestDef.new.lessLogicalAnd()

# expectvalue true
PrecedTestDef.new.lessLogicalOr()

# expectvalue false
PrecedTestDef.new.greaterLogicalAnd()

# expectvalue true
PrecedTestDef.new.greaterLogicalOr()

# expectvalue false
PrecedTestDef.new.equalsLogicalAnd()

# expectvalue true
PrecedTestDef.new.equalsLogicalOr()

# expectvalue false
PrecedTestDef.new.notEqualsLogicalAnd()

# expectvalue true
PrecedTestDef.new.notEqualsLogicalOr()
