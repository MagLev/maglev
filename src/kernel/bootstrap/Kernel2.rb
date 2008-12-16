module Kernel
  # file Kernel2.rb  , parts of kernel that must be deferred to later
  #  in the bootstrap

  def rand(n=nil)
      if n
        RandomInstance.next(n)
      else
	RandomInstance.next
      end
  end

end
