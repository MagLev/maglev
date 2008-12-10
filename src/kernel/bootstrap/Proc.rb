# ---------------------------------
#   Proc 

def lambda(&b)
  b
end

class Proc
    # Proc is identically  Smalltalk ExecBlock
     
    # Class methods

    class_primitive_nobridge 'new&' , '_newProc:'
    # note special Parser code in .mcz , irForProcNewZeroArgs ,
    #  parser converts Proc.new  with no args to 
    #   b = nil ; Proc.new(&b) 
    # and ExecBlock>>_newProc: issues the error for missing block 

    def self.name
      # override Smalltalk name
      'Proc'
    end

    # Instance methods

    # private primitives for $~ implementation only
    primitive_nobridge '_fetchRubyVcGlobal', '_rubyVcGlobalAt:'
    primitive_nobridge '_setRubyVcGlobal', '_rubyVcGlobalAt:put:'

    primitive_nobridge '[]' , '_rubyCall'
    primitive_nobridge '[]' , '_rubyCall:'
    primitive_nobridge '[]' , '_rubyCall:with:'
    primitive_nobridge '[]' , '_rubyCall:with:with:'
    primitive          '[]*' , '_rubyCall:'

    # call, call:, call::, call::: will be compiled to special bytecodes
    #  and won't use the bridge methods generated for  call*
    primitive          'call*' , '_rubyCall:'

    primitive_nobridge '_numArgs', 'numArgs' 
    primitive_nobridge '_lastStar', 'lastRubyArgIsStar'

    def arity
      na = self._numArgs 
      if (self._lastStar) 
        na = -(na)  # negated (num required args + 1)
      end
      na
    end

    # TODO: binding

    # primitives or defs for call should not be needed,
    #  any compiled send of :call translates to special bytecode

    def inspect
      "#<Proc>"
    end

    def to_proc
      self
    end
end
