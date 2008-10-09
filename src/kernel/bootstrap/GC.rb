module GC
  # The Gemstone in-memory garbage collector runs 
  #  automatically whenever GC is required.
  #  It cannot be disabled.

  def self.start
    # has no effect
  end

  def self.enable
    true
  end

  def self.disable
    # has no effect
    false
  end

  def garbage_collect
    # has no effect
  end

end
