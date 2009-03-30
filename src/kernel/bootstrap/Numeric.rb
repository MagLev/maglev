#---------------------------------
#  Ruby Numeric is identically  Smalltalk Number

class Numeric

  def coerce(param)
    raise TypeError, 'numeric coercion failed'
    [ nil, nil]
  end

    primitive '<=>', '_rubyCompare:'

# unaries  +@  -@  eliminated during IR generation by compiler

    primitive 'abs', 'abs'
   
    def ceil
      f = Type.coerce_to(self, Float, :to_f)
      f.ceil
    end

    def div(arg)
      q = self / arg
      q.to_int
    end

    def quo(arg)
      self / quo
    end

    def divmod(arg)
      a = Type.coerce_to(arg, Float, :to_f)
      q = (self / a).floor
      r = self - (q * a)
      [ q, r ]
    end  

# eql?  implemented in subclasses
#  floor implemented in subclasses

        primitive 'hash'

    primitive 'integer?', '_isInteger'

#  nonzero?  implemented in subclasses
#  quo   implemented in subclasses

    def modulo(arg)
      (self.divmod(arg))[1]
    end

# round implemented in subclasses

    primitive 'step', '_rubyTo:by:do:'

    primitive_nobridge '_inspect', 'printString'

    def inspect(touchedSet=nil)
      _inspect
    end


# to_int  implemented in subclasses
# truncated implemented in subclasses
# zero?  implemented in subclasses

    def -@
        self * -1
    end
end

