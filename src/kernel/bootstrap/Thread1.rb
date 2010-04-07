#  file Thread1.rb

class Thread
  KERNEL_SRC_REGEXP = /src\/kernel/

  ThreadCriticalMutex = __resolve_smalltalk_global( :RubyThreadCriticalMutex)
  class ThreadCriticalMutex 

    # initializes/returns the one instance from   SessionTemps current 
    class_primitive_nobridge '__instance', 'instance'

    primitive_nobridge 'locked?', 'isLocked'

    primitive_nobridge 'try_lock', 'tryLock'

    primitive_nobridge 'lock', 'wait'

    primitive_nobridge 'unlock', 'signal'

    def self.new
      raise 'instance creation not allowed'
    end

    def self.locked_by(a_thread)
      self.__instance.locked_by(a_thread)
    end

    def self.locked?
      self.__instance.locked?
    end
  
    def locked_by(a_thread)
      @_st_owner._equal?(a_thread) && self.locked?
    end

    def self.attempt_lock(a_thread)
      self.__instance.attempt_lock(a_thread)
    end

    def attempt_lock(a_thread)
      own = @_st_owner
      if own._equal?(nil)
        if self.try_lock
          @_st_owner = a_thread
          return true 
        end 
      elsif own._equal?(a_thread)
        return true
      end
      false
    end

    def self.unlock_by(a_thread)
      self.__instance.unlock_by(a_thread)
    end

    def unlock_by(a_thread)
      own = @_st_owner
      if own._equal?(a_thread)
        @_st_owner = nil
        self.unlock
      elsif own._equal?(nil)
        # do nothing
      elsif own.__is_terminated
        @_st_owner = nil
        self.unlock
      else
        raise 'cannot unlock, owned by another thread'
      end
    end 

  end
end
