# ---------------------------------
#  Bignum and Integer
#
# The Smalltalk hierarchy is
#      Integer
#        LargeInteger
#        SmallInteger
#

class Integer

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
        primitive_nobridge '+', '+'
        primitive_nobridge '-', '-'
        primitive_nobridge '*', '*'
        primitive_nobridge '/', '_rubyDivide:'

#   Ruby  %   maps to  Smalltalk #'\\'
        primitive_nobridge '%', '\\\\'

        primitive_nobridge '**' , 'raisedTo:'

        primitive_nobridge '|', 'bitOr:'
        primitive_nobridge '&', 'bitAnd:'
        primitive_nobridge '^', 'bitXor:'
        primitive_nobridge '<<', 'bitShift:'
        primitive_nobridge '>>', '_bitShiftRight:'

#  <=> inherited from Numeric

        primitive_nobridge '[]', 'bitAt:'

#  abs inherited from Numeric

        primitive_nobridge '==', '='

        primitive 'eql?', '_ruby_eqlQ:'

        primitive 'div', '_rubyDivide:'

# divmod inherited from Numeric

        primitive 'hash'

#    modulo   maps to Smalltalk  #'\\'
        primitive 'modulo', '\\\\'

        primitive 'quo', '_rubyQuo:'

#  remainder  inherited from numeric

        primitive 'size', 'size'
        primitive 'to_f', 'asFloat'
        primitive 'to_i', 'truncated'
        primitive 'to_int' , 'truncated'
        primitive 'to_s', 'asString'
        primitive 'truncate' , 'truncated'

#  methods from Numeric
        primitive 'coerce', '_rubyCoerce:'
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
