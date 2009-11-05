class Mutex
  # Mutex is identically the Smalltalk class RubyTransientMutex

  class_primitive_nobridge '__new', 'forRubyMutualExclusion'

  def self.new(*args, &blk)
    # first variant gets bridge methods
    m = self.__new
    m.initialize(*args, &blk)
    m
  end

  def self.new
    # subsequent variants replace just the corresponding bridge method
    #  this variant is optimization for most common usage
    m = self.__new
    m.initialize
    m
  end

  primitive_nobridge 'locked?', 'isLocked'

  primitive_nobridge '__trylock', 'tryLock'
  primitive_nobridge '__lock', 'wait'
  primitive_nobridge '__unlock', 'signal'

  def lock
    self.__lock
    @owner = Thread.current
    self
  end

  def try_lock
    if self.__trylock
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
      self.__unlock
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
      self.__unlock
      @owner = nil
    end
  end
end
