module Kernel
  # file Kernel2.rb  , parts of kernel that must be deferred to later
  #  in the bootstrap

  def lambda(&blk)
    Proc.new_lambda(&blk)
  end

  def proc(&blk)
    Proc.new(&blk)
  end

  def rand(n=nil)
      if n
        RandomInstance.next(n)
      else
	RandomInstance.next
      end
  end

end
