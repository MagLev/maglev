module Kernel
  # file Kernel2.rb  , parts of kernel that must be deferred to later
  #  in the bootstrap

  # argument to _bindingContext: is number of frames up from sender
  #   of Kernel.binding from which to create the binding .
  #  Parser has to know about methods which may send _binding_ctx,
  #  to ensure such methods are created with a VariableContext.
  #  see setSendsBinding and the *BindingNode , *EvalNode classes in .mcz
  primitive_nobridge '_binding_ctx' , '_bindingContext:'

  def binding
    # the block argument is synthesized by the parser and should not
    #  be explicitly passed, so this variant probably not used
    Binding.new( self._binding_ctx(0), self, nil )
  end

  def binding(&blk)
    Binding.new( self._binding_ctx(0), self , blk)
  end

  def lambda(&blk)
    Proc.new_lambda(&blk)
  end

  def proc(&blk)
    Proc.new(&blk)
  end

  def rand(n=nil)
    if n
      RandomInstance.next(n) - 1
    else
      RandomInstance.next
    end
  end

end
