#
# Tests the following:
#    block/iteration

# Class definition for block/iterator tests

class IterTest
    # Expected value: xxx
    def simple
        def iter
            yield
            yield
            yield
        end
        iter {print "x"}
        puts
    end

    # Expected value: x*num
    def simpleVar(num)
        def iter(i)
            for i in 1..i
                yield
            end
        end
        iter(num) {print "x"}
        puts
    end

    # Expected value: fibonacci series to num
    def fib(num)
        def fibToMax(max)
            i1, i2  = 1, 1
            while i1 <= max
                yield i1
                i1, i2 = i2, i1+i2
            end
        end
        fibToMax(num) {|f| print f,"|"}
        puts
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

# expectvalue 'xxx'
IterTest.new.simple()

# expectvalue 'xxxxx'
IterTest.new.simpleVar(5)

# expectvalue ''
IterTest.new.simpleVar(-1)

# expectvalue 'xxxxxxxxxx'
IterTest.new.simpleVar(10)

# expectvalue 1|1|2|3|
IterTest.new.fib(3)

# expectvalue 1|1|2|3|5|8|
IterTest.new.fib(9)
