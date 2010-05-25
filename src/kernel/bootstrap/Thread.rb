# Maps to Smalltalk class GsProcess .  See Globals.rb
class Thread
  # constants are defined in Thread1.rb

  primitive_nobridge '[]', 'threadDataAt:'

  primitive_nobridge '[]=', 'threadDataAt:put:'

  class_primitive_nobridge '__abort_on_exception', 'rubyExitOnException:'

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

  # eval support
  def self.__atEvalBinding_put(symbol, value )
    # called by generated code
    # returns value . used only for temps created by an eval 
    cx = self.current[  :__evalArgs ]
    unless cx._equal?(nil)
      binding = cx[0]
      unless binding._equal?(nil)
        binding.__put_temp( symbol, value )
      end
    end
    value
  end

  def self.__evalHomeMethod	# added for 1.8.7
    # called by generated code
    res = nil
    cx = self.current[  :__evalArgs ]
    unless cx._equal?(nil)
      binding = cx[0]
      unless binding._equal?(nil)
        res = binding.__home_method
      end
    end
    res.__name
  end

  class_primitive_nobridge '__stbacktrace', 'backtraceToLevel:'

  def self.__backtrace(includeSt, limit)
    unless limit._isFixnum
      raise ArgumentError, 'limit must be a Fixnum'
    end
    unless limit > 0
      raise ArgumentError, 'limit must be > 0'
    end
    result = []
    maglev_home = ENV['MAGLEV_HOME']
    res_start_ofs = 1
    ststack = __stbacktrace(limit)
    for idx in 0..(ststack.length - 1) do
      file, line, meth = __frame_info(ststack[idx])

      # Kernel methods need to refer to the next app level stack info, not
      # to the src/kernel/* files.  This should almost always be the next
      # frame, but eval and block frames may require searching further.
      # This search is a bit sketchy...
      if file =~ KERNEL_SRC_REGEXP
        file1, line1, meth1 = __find_next_user_info_for(idx+1, ststack)
        file, line = file1, line1 if file1  # don't replace meth
      end
      meth = ":in `#{meth}'" unless meth._equal?(nil) 
      result << "#{file}:#{line}#{meth}"  if file 
    end
    result[(res_start_ofs+1)..-1]
  end

  # If the current stack frame represents kernel source
  # ($MAGLEV_HOME/src/kernel/*), then this frame should be reported from
  # the calling frame (if it is a user frame).  This mimics how MRI reports
  # frames representing C code.
  def self.__find_next_user_info_for(start, stack)
    for idx in (start..stack.length-1) do
      file, line, meth = __frame_info(stack[idx])
      return [file, line, meth] if file and file !~ KERNEL_SRC_REGEXP
    end
    return nil  # failed
  end

  # Return an array of [file_name, line_number, method_name] for the stack
  # frame for ruby stack frames.  If include_st is true, then also return
  # the same information for smalltalk frames.  Ignores env 2.
  def self.__frame_info(stack_frame, include_st=false)
    where, line, source = stack_frame

    if /.*>> (.*?):*\*?&? \(envId 1\)/ =~ where
      # Process a ruby stack frame.
      # Treat _compfileFile methods as top level calls (i.e., no 'in' part)
      meth = $1
      meth = nil if meth =~ /_compileFile/
      if source
        # get baseline and file name from comment at end of method's source
        lines = source.split("\n").grep(/# method/)
        unless lines.empty?
          if /line (\d+) .* file (.*)/=~ lines[-1]
            baseline = $1.to_i   
            file = $2
            # baseline and line are both 1-based , so -1 here
            lnum = baseline + line - 1 
            return [file[0..-2], lnum , meth]
          end
        end
      end
    elsif include_st
      # Process a smalltalk stack frame
      if  /(.*) \(envId 0\)/ =~ where
        meth = $1
        return ["smalltalk", line, meth]
      else
        return ["smalltalk", line, where]# usually in Executed Code
      end
    end
    return nil
  end

  class << self
    private :__find_next_user_info_for, :__frame_info
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
    self.start(*[], &block)
  end

  primitive_nobridge 'group' , 'rubyGroup'

  primitive_nobridge 'inspect', '_rubyInspect'

  primitive_nobridge '__is_terminated', '_isTerminated'

  primitive_nobridge '__join_group', '_joinGroup:'

  # def join(limit); end #
  #  if limit is zero, join will return immediately
  primitive_nobridge 'join', '_join:'

  def join
    #   wait forever for the receiver to finish
    self.value
    self
  end

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

  class_primitive_nobridge 'main', 'main'

  class_primitive_nobridge '__basic_new', 'rubyBasicNew'

  def self.new(*args, &block)
    thr = self.__basic_new
    thr.initialize(*args)
    thr.__start(*args, &block)
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

  class_primitive 'start*&', 'rubyStart:block:'

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


