#
# Tests the following:
#    simple while loop
#    simple while loop with do
#    Compound while loop
#    While with next
#    While with retry
#    While with redo
#    While with break

# Class definition for while tests

class WhileTest
    # Expected value: <stop>
    def simple (start, stop)
        count = start
        while count < stop
            count += 1
        end
        return count
    end

    # Expected value: <stop>
    def simpleDo (start, stop)
        count = start
        while count < stop do
            count += 1
        end
        return count
    end

    # Expected value: <stop-start>
    def compound (start, stop)
        num = 0
        while start < stop
            while start < stop
               num += 1
               start += 1
            end
            start += 1
        end
        return num
    end

    # Expected value: <stop-start-1>
    def next (start, stop)
        orig = start
        num = 0
        while start < stop
            start += 1
            next if start == orig+1
            num += 1
        end
        return num
    end

    # Expected value: <stop-start>
    def redo (start, stop)
        num = 0
        while start < stop
            num += 1
            redo if num == start
            start += 1
        end
        return num
    end

    # Expected value: <stop-start>
    def retry (start, stop)
        num = 0
        while start < stop
            num += 1
            retry if num == start
            start += 1
        end
        return num
    end

    # Expected value: <2>
    def break (start, stop)
        num = 0
        orig = start
        while start < stop
            num += 1
            break if start == orig+1
            start += 1
        end
        return num
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
puts WhileTest.new.simple(1, 10)

# expectvalue 99
puts WhileTest.new.simple(50, 99)

# expectvalue 10
puts WhileTest.new.simpleDo(1, 10)

# expectvalue 99
puts WhileTest.new.simpleDo(50, 99)

# expectvalue 100
puts WhileTest.new.compound(0,100)

# expectvalue 50
puts WhileTest.new.compound(50,100)

# expectvalue 22
puts WhileTest.new.next(0,23)

# expectvalue 749
puts WhileTest.new.next(250,1000)

# expectvalue 10
puts WhileTest.new.redo(10, 20)

# expectvalue 49
puts WhileTest.new.redo(0, 49)

# expectvalue 15
puts WhileTest.new.retry(15, 30)

# expectvalue 51
puts WhileTest.new.retry(0, 51)

# expectvalue 2
puts WhileTest.new.break(15, 30)

# expectvalue 2
puts WhileTest.new.break(0, 51)
