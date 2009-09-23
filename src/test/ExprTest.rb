#
# Tests the following:
#    Add
#

# Class definition for expression tests

def compareFloat( a , exp )
  d = a - exp 
  if (d < 0.0 )
    d = 0.0 - d
  end
  maxD = exp * 1.0e-14 
  ok = d < maxD
  if (ok)
    # ok
  else
    nil.pause
  end
  ok
end

class ExprTest
    # Expected value: 7
    def addIntSimple
         3+4
    end

    # Expected value: 12
    def addIntCompound
         3+4+5
    end

    # Expected value: -2
    def subtractIntSimple
         3-5
    end

    # Expected value: 5
    def subtractIntCompound
         100-90-5
    end

    # Expected value: 150
    def multiplyIntSimple
         50*3
    end

    # Expected value: 150
    def multiplyIntCompound
         25*3*2
    end

    # Expected value: 3
    def divideIntSimple
         300/100
    end

    # Expected value: 111
    def divideIntCompound
         999/3/3
    end

    # Expected value: 7.4
    def addFloatSimple
         3.1 + 4.3
    end

    # Expected value: 12.8
    def addFloatCompound
         3.1+4.3+5.4
    end

    # Expected value: 5.4
    def subtractFloatSimple
         70.5-65.1
    end

    # Expected value: 5.14
    def subtractFloatCompound
         100.54-90.1-5.3
    end

    # Expected value: 39.06
    def multiplyFloatSimple
         12.6*3.1
    end

    # Expected value: 150
    def multiplyFloatCompound
         25.25*3.7*2.4
    end

    # Expected value: 3
    def divideFloatSimple
         255.64/3.6
    end

    # Expected value: 111
    def divideFloatCompound
         3.2/2.3/0.5
    end

    # Expected value: 2
    def modulusInt
         8%3
    end

    # Expected value: 3.2
    def modulusFloat
         25.4%3.7
    end

    # Expected value: 1000
    def powerInt
         10**3
    end

    # Expected value: 54798.1281
    def powerFloat
         15.3**4
    end

    # Expected value: 12
    def rightShift
         100>>3
    end

    # Expected value: 800
    def leftShift
         100<<3
    end

    # Expected value: reverse of a and b
    def parallel(a, b)
        a, b = b, a
         "#{a}#{b}"
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
ret = ExprTest.new.addIntSimple()
raise "ERROR" unless ret == 7

# expectvalue 12
ret = ExprTest.new.addIntCompound()
raise "ERROR" unless ret == 12

# expectvalue -2
ret = ExprTest.new.subtractIntSimple()
raise "ERROR" unless ret == -2

# expectvalue 5
ret = ExprTest.new.subtractIntCompound()
raise "ERROR" unless ret == 5

# expectvalue 150
ret = ExprTest.new.multiplyIntSimple()
raise "ERROR" unless ret == 150

# expectvalue 150
ret = ExprTest.new.multiplyIntCompound()
raise "ERROR" unless ret == 150

# expectvalue 3
ret = ExprTest.new.divideIntSimple()
raise "ERROR" unless ret == 3

# expectvalue 111
ret = ExprTest.new.divideIntCompound()
raise "ERROR" unless ret == 111

# expectvalue 7.4
ret = ExprTest.new.addFloatSimple()
raise "ERROR" unless compareFloat( ret , 7.4 )

# expectvalue 12.8
ret = ExprTest.new.addFloatCompound()
raise "ERROR" unless compareFloat(ret, 12.8)

# expectvalue 5.4
ret = ExprTest.new.subtractFloatSimple()
raise "ERROR" unless compareFloat(ret , 5.4)

# expectvalue 5.14
ret = ExprTest.new.subtractFloatCompound()
raise "ERROR" unless compareFloat(ret , 5.14)

# expectvalue 39.06
ret = ExprTest.new.multiplyFloatSimple()
raise "ERROR" unless compareFloat(ret , 39.06)

# expectvalue 224.22
ret = ExprTest.new.multiplyFloatCompound()
raise "ERROR" unless compareFloat(ret , 224.22)

# expectvalue 71.0111111111111
ret = ExprTest.new.divideFloatSimple()
raise "ERROR" unless compareFloat(ret , 71.0111111111111)

# expectvalue 2.78260869565217
ret = ExprTest.new.divideFloatCompound()
raise "ERROR" unless compareFloat(ret , 2.78260869565217)

# expectvalue 2
ret = ExprTest.new.modulusInt()
raise "ERROR" unless ret == 2

# expectvalue 3.2
ret = ExprTest.new.modulusFloat()
raise "ERROR" unless compareFloat(ret, 3.2 )

# expectvalue 1000
ret = ExprTest.new.powerInt()
raise "ERROR" unless ret == 1000

# expectvalue 54798.1281
ret = ExprTest.new.powerFloat()
raise "ERROR" unless compareFloat(ret , 54798.1281 )

# expectvalue 12
ret = ExprTest.new.rightShift()
raise "ERROR" unless ret == 12

# expectvalue 800
ret = ExprTest.new.leftShift()
raise "ERROR" unless ret == 800

# expectvalue bbbaaa
ret = ExprTest.new.parallel("aaa", "bbb")
raise "ERROR" unless ret == "bbbaaa"
