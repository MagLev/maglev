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
puts IfTest.new.greaterThanInt

# expectvalue true
puts IfTest.new.lessThanInt

# expectvalue true
puts IfTest.new.equalsIntTrue

# expectvalue false
puts IfTest.new.equalsIntFalse

# expectvalue true
puts IfTest.new.notEqualsIntTrue

# expectvalue false
puts IfTest.new.notEqualsIntFalse

# expectvalue true
puts IfTest.new.greaterThanFloat

# expectvalue true
puts IfTest.new.lessThanFloat

# expectvalue true
puts IfTest.new.equalsFloatTrue

# expectvalue false
puts IfTest.new.equalsFloatFalse

# expectvalue true
puts IfTest.new.notEqualsFloatTrue

# expectvalue false
puts IfTest.new.notEqualsFloatFalse

# expectvalue true
puts IfTest.new.greaterThanStringLc

# expectvalue true
puts IfTest.new.lessThanStringLc

# expectvalue true
puts IfTest.new.equalsStringLcTrue

# expectvalue false
puts IfTest.new.equalsStringLcFalse

# expectvalue true
puts IfTest.new.notEqualsStringLcTrue

# expectvalue false
puts IfTest.new.notEqualsStringLcFalse

# expectvalue true
puts IfTest.new.greaterThanStringUc

# expectvalue true
puts IfTest.new.lessThanStringUc

# expectvalue true
puts IfTest.new.equalsStringUcTrue

# expectvalue false
puts IfTest.new.equalsStringUcFalse

# expectvalue true
puts IfTest.new.greaterThanStringMixed

# expectvalue true
puts IfTest.new.lessThanStringMixed
