module GC
  # The Maglev in-memory garbage collector runs 
  #  automatically whenever GC is required.
  #  It cannot be disabled.

  def self.start
    # has no effect
  end

  def self.enable
    false # gc is never disabled
  end

  def self.disable
    # has no effect
    false
  end

  def garbage_collect
    # has no effect
  end

  def self.stress	# added for 1.8.7
    false
  end

  def self.stress=(bool)	# added for 1.8.7
    # has no effect
    false  
  end
 

end
