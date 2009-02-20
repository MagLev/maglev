#---------------------------------
#  Ruby Numeric is identically  Smalltalk Number

class Numeric

    primitive '<=>', '_rubyCompare:'

# unaries  +@  -@  eliminated during IR generation by compiler

    primitive 'abs', 'abs'
    primitive 'ceil', '_rubyCeiling'

# coerce is a subclass responsiblity
#   the implementation in Number just generates subclassResponsibility error
    primitive 'coerce', '_rubyCoerce:'


#           Note some of the "implemented in subclasses" might need
#           implementation in Numeric to support user-defined subclasses
# div implemented in subclasses

    primitive 'divmod', '_divmod:'

# eql?  implemented in subclasses
#  floor implemented in subclasses

        primitive 'hash'

    primitive 'integer?', '_isInteger'

#  modulo implemented in subclasses
#  nonzero?  implemented in subclasses
#  quo   implemented in subclasses

    primitive 'remainder', '_rubyRem:'

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

    primitive 'sin'
    primitive 'cos'
    primitive 'tan'
    primitive 'sqrt'
end

