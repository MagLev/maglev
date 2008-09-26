# ---------------------------------
#   Proc 

def lambda(&b)
  b
end

class Proc
    # primitives for call should not be needed,
    #  any send of #call translates to special bytecode

    self.class.primitive_nobridge 'new&' , '_newProc:'
    self.class.primitive 'new' , '_newProc:'
    
    def inspect
      "#<Proc>"
    end

    primitive_nobridge '[]' , 'value:'
    primitive_nobridge '[]' , 'value:value:'
    primitive_nobridge '[]' , 'value:value:value:'
   
    def [](*args)
      call(*args)
    end

end
