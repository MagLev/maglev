#Experimenting with test strategies

class NumericsTest
  
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
#         sprintf("%0.1f", 3.1+4.3)
        3.1+4.3
     end
     
     # Expected value: 12.8
     def addFloatCompound
#         sprintf("%0.1f", 3.1+4.3+5.4)
        3.1+4.3+5.4
     end
     
     # Expected value: 5.4
     def subtractFloatSimple
#         sprintf("%0.1f", 70.5-65.1)
         70.5-65.1
     end
     
     # Expected value: 5.14
     def subtractFloatCompound
#         sprintf("%0.2f", 100.54-90.1-5.3)
         100.54-90.1-5.3
     end
     
     # Expected value: 39.06
     def multiplyFloatSimple
#         sprintf("%2.2f", 12.6*3.1)
         12.6*3.1
     end
     
     # Expected value: 150
     def multiplyFloatCompound
#         sprintf("%3.2f", 25.25*3.7*2.4)
         25.25*3.7*2.4
     end
     
     # Expected value: 3
     def divideFloatSimple
#         sprintf("%2.13f", 255.64/3.6)
         255.64/3.6
     end
     
     # Expected value: 111
     def divideFloatCompound
#         sprintf("%2.14f", 3.2/2.3/0.5)
          3.2/2.3/0.5
     end

    # Expected value: 2
    def modulusInt
        8%3
    end

     # Expected value: 3.2
     def modulusFloat
#         sprintf("%2.1f", 25.4%3.7)
         25.4%3.7
     end

    # Expected value: 1000
    def powerInt
        10**3
    end

     # Expected value: 54798.1281
     def powerFloat
#       sprintf("%5.4f", 15.3**4)
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



