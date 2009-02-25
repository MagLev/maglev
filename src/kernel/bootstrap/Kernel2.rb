module Kernel
  # file Kernel2.rb  , parts of kernel that must be deferred to later
  #  in the bootstrap

  primitive_nobridge '_binding_ctx' , '_bindingContext'

  def binding
    Binding.new( _binding_ctx, self, nil )
  end

  def binding(&blk)
    # the argument is synthesized by the parser and should not
    #  be explicitly passed
    Binding.new( _binding_ctx, self , blk)
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
