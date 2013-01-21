# Maps to Smalltalk class GsProcess .  See Globals.rb
class Thread
  # constants are defined in Thread1.rb

  primitive_nobridge '[]', 'threadDataAt:'

  primitive_nobridge '[]=', 'threadDataAt:put:'

  class_primitive_nobridge '__abort_on_exception', 'rubyExitOnException:'

  def abort_on_exception
    !!self[:RubyExitOnException]
  end

  def abort_on_exception=(bool)
    self[:RubyExitOnException]=bool
  end

  def self.abort_on_exception
    self.__abort_on_exception(nil)
  end

  # If maglev-ruby invoked with -d  and  abort_on_exception=(true) was called,
  # an exception not handled by a rescue will cause execution to stop
  # for debugging at the topaz prompt before exiting the process.
  #
  def self.abort_on_exception=(bool)
    # Returns the argument .
    v = bool ? true : false
    self.__abort_on_exception(v)
    bool
  end

  primitive_nobridge 'alive?' , 'alive'

  def self.allocate
    raise TypeError, 'Thread.allocate not allowed'
  end

  class_primitive_nobridge '__evVcGput', '_rubyEvalVcPutTilde:underscore:'

  class_primitive_nobridge '__stbacktrace', 'backtraceToLevel:'

  def self.__backtrace(include_st, limit)
    unless limit._isFixnum
      raise ArgumentError, 'limit must be a Fixnum'
    end
    unless limit > 0
      raise ArgumentError, 'limit must be > 0'
    end
    res_start_ofs = 1
    st_stack = __stbacktrace(limit)
    result = self.__st_to_rubybacktrace(st_stack, include_st)
    if include_st
      result.__shift # skip frame of __backtrace
    end
    result
  end

  def self.__add_to_backtrace(result, a_frame)
     file = a_frame[0]
     line = a_frame[1]
     meth = a_frame[2]
     meth = ":in `#{meth}'" unless meth._equal?(nil)
     result <<  "#{file}:#{line}#{meth}"
  end

  def self.__st_to_rubybacktrace(st_stack, include_st=nil)
    result = []
    maglev_home = ENV['MAGLEV_HOME']
    if include_st._equal?(nil)
      wlevel = $-W
      include_st =  wlevel._isFixnum && wlevel > 2
    end
    idx = 0
    limit = st_stack.length - 1
    kernel_src = KERNEL_SRC_STR
    while idx < limit
      frame = __frame_info(st_stack[idx] )
      if include_st
        __add_to_backtrace( result, frame )
      else
        type = frame[3]
        if type._equal?( :ruby)
          file = frame[0]
          if file.include?( kernel_src )
            k_idx = idx
            s_idx = idx + 1
            while s_idx < limit - 1
              frame = __frame_info(st_stack[s_idx] )
              type = frame[3]
              if type._equal?(:ruby)
                if frame[0].include?( kernel_src )
                  k_idx = s_idx
                else
                  break  # at the next ruby application frame
                end
              end
              s_idx += 1
            end
            k_frame = __frame_info(st_stack[k_idx])
            __add_to_backtrace( result, k_frame )
            idx = s_idx - 1  # still need to process frame[s_idx - 1]
          else
            __add_to_backtrace( result, frame )
          end
        end
      end
      idx += 1
    end
    result
  end


  # Return an array of [file_name, line_number, method_name, type]
  # type is one of :ruby , :bridge , :smalltalk
  def self.__frame_info(stack_frame)
    line  = stack_frame[1]
    env_id = stack_frame[2]
    home_sel = stack_frame[3] # selector prefix of home
    is_bridge = stack_frame[4]
    file   = stack_frame[5]   # from debug info used by source_location
    baseline = stack_frame[6]
    if env_id._equal?(1)
      if ! is_bridge && file
        # Process a ruby stack frame.
        # Treat _compfileFile methods as top level calls (i.e., no 'in' part)
        if home_sel
          meth = home_sel
          meth = nil if meth._equal?( :__compileFile )
        end
        if home_sel # if source
          if baseline._isFixnum
            lnum = baseline + line - 1 # baseline and line are both 1-based, so -1
          else
            lnum = line
          end
          return [ file, lnum, meth, :ruby ]
        end
      end
      if is_bridge
        return ["bridge", line, home_sel, :bridge ]
      end
      return [ "bridge" , line, stack_frame[0], :bridge]  # such as __rubySend1: ...
    end
    # assume smalltalk
    #  stack_frame[0] is ' aClassName >> selector (envId 0) '
    return ["smalltalk", line, stack_frame[0].sub('(envId 0)','') , :smalltalk ]
  end

  class << self
    private  :__frame_info
  end

  # ThreadCriticalMutex defined in Thread1.rb

  def self.critical
    ThreadCriticalMutex.locked?
  end

  def self.critical=(bool)
    mutex_cls = ThreadCriticalMutex
    me = self.current
    if bool
      mutex_cls.attempt_lock(me)
    else
      mutex_cls.unlock_by(me)
      false
    end
  end

  class_primitive_nobridge 'current', '_current'

  class_primitive_nobridge 'exit', 'exit'
  primitive_nobridge 'exit', 'exit'

  def self.fork(&block)
    thr = self.__basic_new
    # does not call initialize
    thr.__start(*[], &block)
  end

  primitive_nobridge 'group' , 'rubyGroup'

  primitive_nobridge 'inspect', '_rubyInspect'

  primitive_nobridge '__is_terminated', '_isTerminated'

  primitive_nobridge '__join_group', '_joinGroup:'

  def join
    # wait forever for the receiver to finish
    self.value
    self
  end
  #  if limit is zero, join will return immediately
  primitive_nobridge 'join', '_join:'

  primitive_nobridge 'keys', 'keys'

  primitive_nobridge 'key?', 'includesKey:'

  def self.kill(thread)
    if thread._is_a?(Thread)
      thread.__terminate
    else
      raise ArgumentError, 'not a Thread'
    end
  end

  primitive_nobridge '__kill_ex', 'signalException:'

  primitive_nobridge 'kill', 'exit'

  class_primitive_nobridge 'list', 'allProcesses'

  class_primitive_nobridge 'main', 'rubyMain'

  class_primitive_nobridge '__basic_new', 'rubyBasicNew'

  def self.new(*args, &block)
    thr = self.__basic_new
    thr.initialize(*args, &block)
    blk = thr.__block
    thr.__start(*args, &blk)
  end

  def __block
    @_st_block
  end

  def initialize(*args, &block)
    @_st_block = block
  end

  class_primitive_nobridge 'pass', 'pass'

  primitive_nobridge 'priority', 'rubyPriority'

  primitive_nobridge 'priority=', 'rubyPriority:'

  def raise(ex_class, message, *args)
    # TODO  args is callback info not yet implemented
    self.raise(ex_class, message)
  end

  def raise(ex_class, message)
    ex = ex_class.exception
    if self._equal?(Thread.current)
      ex.signal(message)
    else
      ex.__message=(message)
      self.__kill_ex(ex)
    end
  end

  def raise(msg)
    if (msg._isString)
      self.raise(RuntimeError, msg)
    else
      # msg should be a subclass of Exception or
      #  an object that returns a new exception
      ex = msg.exception
      if self._equal?(Thread.current)
        ex.signal
      else
         self.__kill_ex(ex)
      end
    end
  end

  def raise
    self.raise(RuntimeError, '')
  end

  primitive_nobridge 'run' , 'rubyRun'

  def safe_level
    0   # TODO  $SAFE
  end

  primitive_nobridge 'status', 'rubyStatus'

  class_primitive_nobridge '__recursion_guard_set', '_recursionGuardSet'

  primitive_nobridge '__start*&', 'rubyStart:block:'

  def self.start(*args, &block)
    thr = self.__basic_new
    # does not call initialize
    thr.__start(*args, &block)
  end

  primitive_nobridge 'stop?', 'rubyStopped'

  class_primitive_nobridge '__stop', 'stop'

  def self.stop
    self.critical=(false)
    self.__stop
  end

  primitive_nobridge '__terminate', 'terminate'

  primitive_nobridge 'terminate', 'exit'

  primitive_nobridge 'value' , 'joinValue'

  primitive_nobridge 'wakeup', 'rubyResume'

end
