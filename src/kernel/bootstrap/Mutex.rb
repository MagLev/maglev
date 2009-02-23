class Mutex
  # Mutex is identically the Smalltalk class Semaphore

  class_primitive_nobridge '_new', 'forRubyMutualExclusion'

  def self.new(*args, &blk)
    # first variant gets bridge methods
    m = self._new
    m.initialize(*args, &blk)
    m
  end

  def self.new
    # subsequent variants replace just the corresponding bridge method
    #  this variant is optimization for most common usage
    m = self._new
    m.initialize
    m
  end

  primitive_nobridge 'locked?', 'isLocked'

  primitive_nobridge 'try_lock', 'tryLock'

  primitive_nobridge 'lock', 'wait'

  primitive_nobridge 'unlock', 'signal'

  def synchronize( &block )
    self.lock
    begin
      yield
    ensure
      self.unlock
    end
  end
end
