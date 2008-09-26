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

    self.class.primitive_nobridge 'new&' , '_newProc:'
    self.class.primitive 'new' , '_newProc:'
    
    def inspect
      "#<Proc>"
    end

    primitive_nobridge '[]' , '_rubyCall'
    primitive_nobridge '[]' , '_rubyCall:'
    primitive_nobridge '[]' , '_rubyCall:with:'
    primitive_nobridge '[]' , '_rubyCall:with:with:'
    primitive_nobridge '[]*' , '_rubyCall:'
end
