#
# Tests the following:
#    Simple for loop
#    Simple for loop with do
#    Compound for loop
#    For with next
#    For with retry
#    For with redo
#    For with break

# Class definition for for tests

class ForTest
    # Expected value: <stop>
    def simple (start, stop)
        for num in (start..stop)
        end
        return num
    end

    # Expected value: <stop>
    def simpleDo (start, stop)
        for num in (start..stop) do
        end
        return num
    end

    # Expected value: <stop>
    def compound (start, stop)
        for num1 in (start..stop)
            for num2 in (start..stop)
            end
        end
        return num2
    end

    # Expected value: <stop-start-1>
    def next (start, stop)
        num2 = 0
        for num1 in start..stop
            next if num1 == start
            next if num1 == stop
            num2 = num2 + 1
        end
        return num2
    end

    # Expected value: <stop-start+1>
    def redo (start, stop)
        num2 = 0
        for num1 in start..stop
            num2 = num2 + 1
            redo if stop == start
        end
        return num2
    end

    # Expected value: <stop-start+1>
    def retry (start, stop)
        num2 = 0
        for num1 in start..stop
            num2 = num2 + 1
            retry if stop == start
        end
        return num2
    end

    # Expected value: <1>
    def break (start, stop)
        num2 = 0
        for num1 in start..stop
            num2 = num2 + 1
            break if num1 == start
        end
        return num2
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

# expectvalue 10
puts ForTest.new.simple(1, 10)

# expectvalue 20
puts ForTest.new.simple(5, 20)

# expectvalue 10
puts ForTest.new.simpleDo(1, 10)

# expectvalue 20
puts ForTest.new.simpleDo(5, 20)

# expectvalue 10
puts ForTest.new.compound(1, 10)

# expectvalue 20
puts ForTest.new.compound(5, 20)

# expectvalue 49
puts ForTest.new.next(0, 50)

# expectvalue 74
puts ForTest.new.next(25, 100)

# expectvalue 2
puts ForTest.new.redo(49, 50)

# expectvalue 76
puts ForTest.new.redo(25, 100)

# expectvalue 16
puts ForTest.new.retry(35, 50)

# expectvalue 76
puts ForTest.new.retry(25, 100)

# expectvalue 1
puts ForTest.new.break(0,19)

# expectvalue 1
puts ForTest.new.break(75,100)
