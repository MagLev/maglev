#
# Tests the following:
#    int > int
#    int < int
#    int == int
#    int != int 
#    float > float
#    float < float
#    float == float
#    float != float
#    string > string
#    string < string
#    string == string
#    string != string

# Class definition for if tests

class IfTest
    # Expected value: true
    def greaterThanInt
        if 6 > 3
            return true
        else
            return false
        end
    end

    # Expected value: true
    def lessThanInt
        if 3 < 6
            return true
        else
            return false
        end
    end

    # Expected value: true
    def equalsIntTrue
        if 101 == 101
            return true
        else
            return false
        end
    end

    # Expected value: false
    def equalsIntFalse
        if 101 == 102
            return true
        else
            return false
        end
    end

    # Expected value: true
    def notEqualsIntTrue
        if 101 != 102
            return true
        else
            return false
        end
    end

    # Expected value: false
    def notEqualsIntFalse
        if 101 != 101
            return true
        else
            return false
        end
    end

    # Expected value: true
    def greaterThanFloat
        if 9.87654 > 6.7584
            return true
        else
            return false
        end
    end

    # Expected value: true
    def lessThanFloat
        if 3.156 < 6.7584
            return true
        else
            return false
        end
    end

    # Expected value: true
    def equalsFloatTrue
        if 9.87654 == 9.87654
            return true
        else
            return false
        end
    end

    # Expected value: false
    def equalsFloatFalse
        if 9.87654 == 9.87653
            return true
        else
            return false
        end
    end

    # Expected value: true
    def notEqualsFloatTrue
        if 9.87654 != 9.87653
            return true
        else
            return false
        end
    end

    # Expected value: false
    def notEqualsFloatFalse
        if 9.87654 != 9.87654
            return true
        else
            return false
        end
    end

    # Expected value: true
    def greaterThanStringLc
        if 'def' > 'abc'
            return true
        else
            return false
        end
    end

    # Expected value: true
    def lessThanStringLc
        if 'abc' < 'def'
            return true
        else
            return false
        end
    end

    # Expected value: true
    def equalsStringLcTrue
        if 'xyz' == 'xyz'
            return true
        else
            return false
        end
    end

    # Expected value: false
    def equalsStringLcFalse
        if 'abc' == 'def'
            return true
        else
            return false
        end
    end

    # Expected value: true
    def notEqualsStringLcTrue
        if 'xyz' != 'abc'
            return true
        else
            return false
        end
    end

    # Expected value: false
    def notEqualsStringLcFalse
        if 'abc' != 'abc'
            return true
        else
            return false
        end
    end

    # Expected value: true
    def greaterThanStringUc
        if 'DEF' > 'ABC'
            return true
        else
            return false
        end
    end

    # Expected value: true
    def lessThanStringUc
        if 'ABC' < 'DEF'
            return true
        else
            return false
        end
    end

    # Expected value: true
    def equalsStringUcTrue
        if 'XYZ' == 'XYZ'
            return true
        else
            return false
        end
    end

    # Expected value: false
    def equalsStringUcFalse
        if 'ABC' == 'DEF'
            return true
        else
            return false
        end
    end

    # Expected value: true
    def greaterThanStringMixed
        if 'Def' > 'AbC'
            return true
        else
            return false
        end
    end

    # Expected value: true
    def lessThanStringMixed
        if 'AbC' < 'Def'
            return true
        else
            return false
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

# expectvalue true
ret = IfTest.new.greaterThanInt
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.lessThanInt
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.equalsIntTrue
raise "ERROR" unless ret == true

# expectvalue false
ret = IfTest.new.equalsIntFalse
raise "ERROR" unless ret == false

# expectvalue true
ret = IfTest.new.notEqualsIntTrue
raise "ERROR" unless ret == true

# expectvalue false
ret = IfTest.new.notEqualsIntFalse
raise "ERROR" unless ret == false

# expectvalue true
ret = IfTest.new.greaterThanFloat
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.lessThanFloat
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.equalsFloatTrue
raise "ERROR" unless ret == true

# expectvalue false
ret = IfTest.new.equalsFloatFalse
raise "ERROR" unless ret == false

# expectvalue true
ret = IfTest.new.notEqualsFloatTrue
raise "ERROR" unless ret == true

# expectvalue false
ret = IfTest.new.notEqualsFloatFalse
raise "ERROR" unless ret == false

# expectvalue true
ret = IfTest.new.greaterThanStringLc
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.lessThanStringLc
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.equalsStringLcTrue
raise "ERROR" unless ret == true

# expectvalue false
ret = IfTest.new.equalsStringLcFalse
raise "ERROR" unless ret == false

# expectvalue true
ret = IfTest.new.notEqualsStringLcTrue
raise "ERROR" unless ret == true

# expectvalue false
ret = IfTest.new.notEqualsStringLcFalse
raise "ERROR" unless ret == false

# expectvalue true
ret = IfTest.new.greaterThanStringUc
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.lessThanStringUc
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.equalsStringUcTrue
raise "ERROR" unless ret == true

# expectvalue false
ret = IfTest.new.equalsStringUcFalse
raise "ERROR" unless ret == false

# expectvalue true
ret = IfTest.new.greaterThanStringMixed
raise "ERROR" unless ret == true

# expectvalue true
ret = IfTest.new.lessThanStringMixed
raise "ERROR" unless ret == true
