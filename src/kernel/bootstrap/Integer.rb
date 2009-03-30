# ---------------------------------
#  Bignum and Integer
#
# The Smalltalk hierarchy is
#      Integer
#        LargeInteger
#        SmallInteger
#

class Integer

  def coerce(param)
    begin
      v = param.to_int
      if v._isInteger
        return [ v, self ]
      end
    rescue
      # continue execution
    end
    super
  end

    # following 3 prims contain handler for RubyBreakException
    primitive 'times&', '_rubyTimes:'
    # def times(&block) ; end 

    # def upto(n, &block) ; end 
    primitive 'upto&', '_rubyUpto:block:'

    # def downdo(n, &block) ; end 
    primitive 'downto&', '_rubyDownto:block:'


    def chr
        if self > 255
            raise RangeError, "#{self} out of char range"
        end
        string = ' '
        string[0] = self
        string
    end

    def next
      self + 1
    end

    def succ
      self + 1
    end
        primitive_nobridge '+', '_rubyAdd:'
        primitive_nobridge '-', '_rubySubtract:'
        primitive_nobridge '*', '_rubyMultiply:'
        primitive_nobridge '/', '_rubyDivide:'

        primitive_nobridge '%', '_rubyModulus:'

        primitive_nobridge '**', '_rubyRaisedTo:'

        def _fraised_to(arg)
          # handles coercion for _rubyRaisedTo:
          if arg._isInteger 
            raise TypeError , 'coercion error in ** '
          else
            s = Type.coerce_to(self, Float, :to_f)
            s ** arg 
          end
        end 

        primitive_nobridge '&', '_rubyBitAnd:'
        primitive_nobridge '|', '_rubyBitOr:'
        primitive_nobridge '^', '_rubyBitXor:'
        primitive_nobridge '<<', '_rubyShiftLeft:'

        def >>(arg)
          self << ( 0 - arg )
        end

       # following handle primitive failures of  _rubyBitOr:, etc
       def _bit_and(arg)
         a = Type.coerce_to(arg, Integer, :to_int) 
         self & a 
       end

       def _bit_or(arg)
         a = Type.coerce_to(arg, Integer, :to_int) 
         self | a 
       end

       def _bit_xor(arg)
         a = Type.coerce_to(arg, Integer, :to_int) 
         self ^ a 
       end

       def _shift_left(arg)
         a = Type.coerce_to(arg, Integer, :to_int) 
         self << a 
       end

     primitive '<',  '_rubyLt:'
     primitive '<=', '_rubyLteq:'
     primitive '>' , '_rubyGt:'
     primitive '>=', '_rubyGteq:'
     primitive '==', '_rubyEqual:'

    #  <=> inherited from Numeric

        primitive_nobridge '[]', 'bitAt:'

#  abs inherited from Numeric

     def ceil
       self
     end

        primitive 'eql?', '_ruby_eqlQ:'

        alias div /

        def divmod(arg)
          if arg._isInteger
            q = self / arg
            r = self - (q * arg)
            [ q, r ]
          else
            super
          end
        end

        primitive 'hash'

        alias modulo %

        def quo(param)
           (self.to_f ) / param
        end

#  remainder  inherited from numeric

        primitive 'size', 'size'
        primitive 'to_f', 'asFloat'
        primitive 'to_i', 'truncated'
        primitive 'to_int' , 'truncated'
        primitive 'to_s', 'asString'
        primitive 'truncate' , 'truncated'

#  methods from Numeric
        primitive 'floor', 'floor'
        primitive 'nonzero?', '_rubyNonzero'
        primitive 'round', 'rounded'
        primitive 'zero?', '_rubyEqualZero'

        primitive_nobridge 'step&', 'to:do:'
        primitive_nobridge 'step&', 'to:by:do:'


# Were in String.rb
    def _split_string(string, limit)
        self.chr._split_string(string, limit)
    end

    # deleted _index_string
end
