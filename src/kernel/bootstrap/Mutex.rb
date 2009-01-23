class Mutex
  # Mutex is identically the Smalltalk class Semaphore

  def self.new
    'Mutex'  # override Smalltalk name
  end

  class_primitive_nobridge 'new', 'forMutualExclusion'

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
