class Range
  class FlipFlop
    # runtime support for RubyFlipNode 

    # instance creation done via generated code , 
    #  initialization is by Smalltalk methods in .mcz
    def initialize
      raise ArgumentError, 'instances created only by Smalltalk runtime code'
    end

    # instances are stored in a method temporary in the home context of
    # the method containing the RubyFlipNode .
    # instances created by generated code when the RubyFlipNode first
    #  evaluated during the method .

    def __update_from_to(expr1val, expr2val)
      # runtime for 2 dots form
      s = @_st_theState
      unless s
        if expr1val[]
          s = true
          new_state = s
          if expr2val[]
            new_state = false
          end
          @_st_theState = new_state
        end
      else
        if expr2val[]
          new_state = false
          @_st_theState = new_state
        end
      end
      s
    end

    def __update_from_to3(expr1val, expr2val)
      # runtime for 3 dots form
      s = @_st_theState
      unless s
        if expr1val[]
          s = true
          @_st_theState = s
        end
      else
        if expr2val[]
          @_st_theState = false 
        end
      end
      s
    end

  end
end
