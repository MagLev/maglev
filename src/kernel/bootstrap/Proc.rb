# ---------------------------------
#   Proc 

def lambda(&b)
  b
end

class Proc
    # Proc is identically  ExecBlock
    #
    # primitives for call should not be needed,
    #  any send of #call translates to special bytecode

    class_primitive_nobridge 'new&' , '_newProc:'
    # note special Parser code in .mcz , irForProcNewZeroArgs ,
    #  parser converts Proc.new  with no args to 
    #   b = nil ; Proc.new(&b) 
    # and ExecBlock>>_newProc: issues the error for missing block 
    
    primitive_nobridge '[]' , '_rubyCall'
    primitive_nobridge '[]' , '_rubyCall:'
    primitive_nobridge '[]' , '_rubyCall:with:'
    primitive_nobridge '[]' , '_rubyCall:with:with:'
    primitive_nobridge '[]*' , '_rubyCall:'

    def self.name
      # override Smalltalk name
      'Proc'
    end

    def inspect
      "#<Proc>"
    end
end
