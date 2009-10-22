class Mutex
  # Mutex is identically the Smalltalk class RubyTransientMutex

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

  primitive_nobridge '_trylock', 'tryLock'
  primitive_nobridge '_lock', 'wait'
  primitive_nobridge '_unlock', 'signal'

  def lock
    self._lock
    @owner = Thread.current
    self
  end

  def try_lock
    if self._trylock
      @owner = Thread.current
      return true
    end
    false  
  end

  def unlock
    if locked?
      unless @owner.equal?(Thread.current)
        raise ThreadError, 'Mutex#unlock, not owned by current thread'
      end
      self._unlock
      @owner = nil
    else
      raise ThreadError, 'Mutex#unlock, the mutex is not locked'
    end
  end

  def synchronize( &block )
    self.lock
    begin
      yield
    ensure
      self._unlock
      @owner = nil
    end
  end
end
