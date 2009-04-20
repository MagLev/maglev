# ~
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
    unless param._isSymbol
      begin
        v = param.to_int
        if v._isInteger
          return [ v, self ]
        end
      rescue
        # continue execution
      end
    end
    super
  end

  def self.induced_from(obj)
    if obj._isInteger
      obj
    elsif obj._isFloat
      obj.to_i
    else
      raise TypeError, "argument to induced_from neither Float nor Integer"
      nil
    end
  end

    # following 3 prims contain handler for RubyBreakException
    primitive 'times&', '_rubyTimes:'
    # def times(&block) ; end 

    # def upto(n, &block) ; end 
    primitive 'upto&', '_rubyUpto:block:'

    # def downdo(n, &block) ; end 
    primitive 'downto&', '_rubyDownto:block:'


    def chr
        if self > 255 || self < 0
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
        primitive_nobridge '_divide', '_rubyDivide:'

        primitive_nobridge '%', '_rubyModulo:'
        primitive_nobridge 'modulo', '_rubyModulo:'

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

        primitive_nobridge '~', 'bitInvert'
        primitive_nobridge '&', '_rubyBitAnd:'
        primitive_nobridge '|', '_rubyBitOr:'
        primitive_nobridge '^', '_rubyBitXor:'
        primitive_nobridge '<<', '_rubyShiftLeft:'

        def >>(arg)
          unless arg._isFixnum
            if arg._isInteger 
              if (self >= 0)
                return 0
              else
                return -1
              end
            end
          end
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
         unless a._isFixnum
           raise RangeError, 'argument must be a Fixnum'
         end
         self << a 
       end

     primitive '<',  '_rubyLt:'
     primitive '<=', '_rubyLteq:'
     primitive '>' , '_rubyGt:'
     primitive '>=', '_rubyGteq:'
     primitive '==', '_rubyEqual:'

    #  <=> inherited from Numeric

     primitive_nobridge '_bit_at', 'bitAt:'
     
     def [](arg)
       a = Type.coerce_to(arg, Integer, :to_int)
       if (a < 0)
         0
       else
         self._bit_at(a)
       end 
     end

#  abs inherited from Numeric

     def ceil
       self
     end

        primitive 'eql?', '_ruby_eqlQ:'

        def divmod(arg)
          if arg._isInteger
            q = self._divide(arg)
            r = self - (q * arg)
            [ q, r ]
          elsif arg._isNumeric
            c = arg.coerce(self)
            c[0].divmod(c[1])
          else
            raise TypeError, 'numeric coercion failed'
            nil
          end
        end

        def div(arg)
          if arg._isFloat 
            if arg == 0.0
              raise FloatDomainError, 'argument to div is zero'
            end
          end
          q = self._divide(arg)
          q.to_int
        end

        primitive 'hash'

        
        def quo(param)
           (self.to_f )._divide(param)
        end

#  remainder  inherited from numeric

        primitive 'size', 'size'
        primitive 'to_f', 'asFloat'
        primitive 'to_i', 'truncated'
        primitive 'to_int' , 'truncated'

        primitive '_to_s_base_show', 'printStringRadix:showRadix:'

        # primitive to_s  is the zero arg form 
        primitive 'to_s', 'asString'

        def to_s(base)
          unless base._isFixnum 
            raise TypeError, 'arg must be a Fixnum'
          end
          _to_s_base_show(base, false)
        end

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
