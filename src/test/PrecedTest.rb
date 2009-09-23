#
# Tests the following:
#    precedence

# Class definition for precedence tests

class PrecedTestDef
    # Expected value: 3775
    def refPower
        array = Array.new(1)
        array[0]=15
        #return array[0]**3
        array[0]**3
    end

    # Expected value: 490
    def powerPlus
        22**2+6
    end

    # Expected value: 478
    def powerMinus
        22**2-6
    end

    # Expected value: 210
    def plusMultiply
        10+20*10
    end

    # Expected value: -190
    def minusMultiply
        10-20*10
    end

    # Expected value: 70
    def plusDivide
        10+20/4
    end

    # Expected value: 5
    def minusDivide
        10-20/4
    end

    # Expected value: 49
    def plusModulus
        49+15%3
    end

    # Expected value: 48
    def minusModulus
        49-15%2
    end

    # Expected value: 12
    def plusRightShift
        100>>2+1
    end

    # Expected value: 800
    def plusLeftShift
        100<<2+1
    end

    # Expected value: 12
    def minusRightShift
        100>>4-1
    end

    # Expected value: 800
    def minusLeftShift
        100<<4-1
    end

    # Expected value: false
    def lessLogicalAnd
        if 3<4&&4<3
            return "true"
        else
            return "false"
        end
    end

    # Expected value: true
    def lessLogicalOr
        if 3<4||4<3
            return "true"
        else
            return "false"
        end
    end

    # Expected value: false
    def greaterLogicalAnd
        if 3>4&&4>3
            return "true"
        else
            return "false"
        end
    end

    # Expected value: true
    def greaterLogicalOr
        if 3>4||4>3
            return "true"
        else
            return "false"
        end
    end

    # Expected value: false
    def equalsLogicalAnd
        if 3==3&&3==4
            return "true"
        else
            return "false"
        end
    end

    # Expected value: true
    def equalsLogicalOr
        if 3==3||4==4
            return "true"
        else
            return "false"
        end
    end

    # Expected value: false
    def notEqualsLogicalAnd
        if !3==3&&!4==4
            return "true"
        else
            return "false"
        end
    end

    # Expected value: true
    def notEqualsLogicalOr
        if !3==3||!4==4
            return "false"
        else
            return "true"
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
ret = PrecedTestDef.new.refPower()
raise "ERROR" unless ret == 3375

# expectvalue 490
ret = PrecedTestDef.new.powerPlus()
raise "ERROR" unless ret == 490

# expectvalue 478
ret = PrecedTestDef.new.powerMinus()
raise "ERROR" unless ret == 478

# expectvalue 210
ret = PrecedTestDef.new.plusMultiply()
raise "ERROR" unless ret == 210

# expectvalue -190
ret = PrecedTestDef.new.minusMultiply()
raise "ERROR" unless ret == -190

# expectvalue 15
ret = PrecedTestDef.new.plusDivide()
raise "ERROR" unless ret == 15

# expectvalue 5
ret = PrecedTestDef.new.minusDivide()
raise "ERROR" unless ret == 5

# expectvalue 49
ret = PrecedTestDef.new.plusModulus()
raise "ERROR" unless ret == 49

# expectvalue 48
ret = PrecedTestDef.new.minusModulus()
raise "ERROR" unless ret == 48

# expectvalue 12
ret = PrecedTestDef.new.plusRightShift()
raise "ERROR" unless ret == 12

# expectvalue 800
ret = PrecedTestDef.new.plusLeftShift()
raise "ERROR" unless ret == 800

# expectvalue 12
ret = PrecedTestDef.new.minusRightShift()
raise "ERROR" unless ret == 12

# expectvalue 800
ret = PrecedTestDef.new.minusLeftShift()
raise "ERROR" unless ret == 800

# expectvalue 'false'
ret = PrecedTestDef.new.lessLogicalAnd()
raise "ERROR" unless ret == 'false'

# expectvalue 'true'
ret = PrecedTestDef.new.lessLogicalOr()
raise "ERROR" unless ret == 'true'

# expectvalue 'false'
ret = PrecedTestDef.new.greaterLogicalAnd()
raise "ERROR" unless ret == 'false'

# expectvalue 'true'
ret = PrecedTestDef.new.greaterLogicalOr()
raise "ERROR" unless ret == 'true'

# expectvalue 'false'
ret = PrecedTestDef.new.equalsLogicalAnd()
raise "ERROR" unless ret == 'false'

# expectvalue 'true'
ret = PrecedTestDef.new.equalsLogicalOr()
raise "ERROR" unless ret == 'true'

# expectvalue 'false'
ret = PrecedTestDef.new.notEqualsLogicalAnd()
raise "ERROR" unless ret == 'false'

# expectvalue 'true'
ret = PrecedTestDef.new.notEqualsLogicalOr()
raise "ERROR" unless ret == 'true'
