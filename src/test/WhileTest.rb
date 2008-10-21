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
        retried = false
        retrylim = (stop - start) / 2 
        num = 0
        while start < stop
            num += 1
            if (num == retrylim && retried == false)
              retried = true
              retry
            end
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
            if start == orig+1 
              break
            end
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
ret = WhileTest.new.simple(1, 10)
raise "ERROR" unless ret == 10

# expectvalue 99
ret = WhileTest.new.simple(50, 99)
raise "ERROR" unless ret == 99

# expectvalue 10
ret = WhileTest.new.simpleDo(1, 10)
raise "ERROR" unless ret == 10

# expectvalue 99
ret = WhileTest.new.simpleDo(50, 99)
raise "ERROR" unless ret == 99

# expectvalue 100
ret = WhileTest.new.compound(0,100)
raise "ERROR" unless ret == 100

# expectvalue 50
ret = WhileTest.new.compound(50,100)
raise "ERROR" unless ret == 50

# expectvalue 22
ret = WhileTest.new.next(0,23)
raise "ERROR" unless ret == 22

# expectvalue 749
ret = WhileTest.new.next(250,1000)
raise "ERROR" unless ret == 749

# expectvalue 10
ret = WhileTest.new.redo(10, 20)
raise "ERROR" unless ret == 10

# expectvalue 49
ret = WhileTest.new.redo(0, 49)
raise "ERROR" unless ret == 49

# retry in a while loop raises RubyBreakException 
#   per MRI un-rescuable   'retry outside of rescue clause'
# begin
#  ret = WhileTest.new.retry(10, 20)
#  raise "ERROR" 
#rescue 
#  
#end

# expectvalue 2
ret = WhileTest.new.break(15, 30)
raise "ERROR" unless ret == 2

# expectvalue 2
ret = WhileTest.new.break(0, 51)
raise "ERROR" unless ret == 2
