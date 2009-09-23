#
# Tests the following:
#    Instantiation

# Class definition for case tests

class CaseTest
    # Expected value: <depends on year entered>
    def case1(year)
        president = case year
            when 1969..1974 then 'Nixon'
            when 1974..1976 then 'Ford'
            when 1977..1980 then 'Carter'
            when 1981..1988 then 'Reagan'
            when 1989..1992 then 'Bush I'
            when 1993..2000 then 'Clinton'
            when 2001..2004 then 'Bush II'
            else "Someone else"
        end
        puts president
    end

    # Expected value: <depends on year entered>
    def case2(year)
        president = case year
            when 1969..1974: 'Nixon'
            when 1974..1976: 'Ford'
            when 1977..1980: 'Carter'
            when 1981..1988: 'Reagan'
            when 1989..1992: 'Bush I'
            when 1993..2000: 'Clinton'
            when 2001..2004: 'Bush II'
            else "Someone else"
        end
        puts president
    end

    # Expected value: <depends on num entered>
    def case3(num)
        case num
            when 1
                puts "1"
            when 2
                puts "2"
            when 3
                puts "3"
            else
                puts "?"
        end
    end

    # Expected value: <depends on year entered>
    # Note that case with expressions is not working
    def case4(year)
        leap = case
            when year%400 == 0: true
            when year%100 == 0: false
            when year%4   == 0: true
            else false
        end
        puts leap
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

# expectvalue 'Carter'
CaseTest.new.case1(1979)

# expectvalue 'Nixon'
CaseTest.new.case1(1969)

# expectvalue 'Someone else'
CaseTest.new.case1(2008)

# expectvalue 'Carter'
CaseTest.new.case2(1979)

# expectvalue 'Nixon'
CaseTest.new.case2(1969)

# expectvalue 'Someone else'
CaseTest.new.case2(2008)

# expectvalue 1
CaseTest.new.case3(1)

# expectvalue 2
CaseTest.new.case3(2)

# expectvalue ?
CaseTest.new.case3(4)

# expectvalue true
CaseTest.new.case4(2008)

# expectvalue false
CaseTest.new.case4(2007)

# expectvalue true
CaseTest.new.case4(2000)

# expectvalue false
CaseTest.new.case4(1900)
