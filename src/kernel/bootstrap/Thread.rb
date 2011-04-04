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
  def self.__evVc
    # Thread.current[ :__evalArgs ][0]    is a Binding
    self.current[  :__evalArgs ][0].__context 
  end
  def self.__evArgs
    self.current[  :__evalArgs ]
  end
  def self.__evBnd
    self.current[  :__evalArgs ][0]
  end
  def self.__evBlk
    self.current[  :__evalArgs ][1]
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
    result[(res_start_ofs+1)..-1]
  end

  def self.__st_to_rubybacktrace(st_stack, include_st=nil)
    result = []
    maglev_home = ENV['MAGLEV_HOME']
    if include_st._equal?(nil)
      wlevel = $-W 
      include_st =  wlevel._isFixnum && wlevel > 2 
    end
    for idx in 0..(st_stack.length - 1) do
      f_info = __frame_info(st_stack[idx], include_st)
      if f_info._not_equal?(nil)
        file = f_info[0]
        line = f_info[1]
        meth = f_info[2]
        # Kernel methods need to refer to the next app level stack info, not
        # to the src/kernel/* files.  This should almost always be the next
        # frame, but eval and block frames may require searching further.
        # This search is a bit sketchy...
        if file and file.include?( KERNEL_SRC_STR )
          u_info = __find_next_user_info_for(idx+1, st_stack)
          if u_info._not_equal?(nil)
            file = u_info[0]
            line = u_info[1]
            # don't replace meth
           end
        end
        meth = ":in `#{meth}'" unless meth._equal?(nil) 
        result << "#{file}:#{line}#{meth}"  if file 
      end
    end
    result
  end

  # If the current stack frame represents kernel source
  # ($MAGLEV_HOME/src/kernel/*), then this frame should be reported from
  # the calling frame (if it is a user frame).  This mimics how MRI reports
  # frames representing C code.
  def self.__find_next_user_info_for(start, stack)
    for idx in (start..stack.length-1) do
      f_info = __frame_info(stack[idx])
      if f_info._not_equal?(nil)
        file = f_info[0]
        if file and ! file.include?(KERNEL_SRC_STR)
          line = f_info[1]
          meth = f_info[2]
          return [file, line, meth] 
        end
      end
    end
    return nil  # failed
  end

  # Return an array of [file_name, line_number, method_name] for the stack
  # frame for ruby stack frames.  If include_st is true, then also return
  # the same information for smalltalk frames.  Ignores env 2.
  def self.__frame_info(stack_frame, include_st=false)
    where = stack_frame[0]
    line  = stack_frame[1]
    env_id = stack_frame[2]
    home_sel = stack_frame[3] # selector prefix of home
    is_bridge = stack_frame[4]
    file   = stack_frame[5]   # from debug info used by source_location
    baseline = stack_frame[6]
    
    # if /.*# (.*?):*\*?&? \(envId 1\)/ =~ where
    if env_id._equal?(1) && ! is_bridge
      # Process a ruby stack frame.
      # Treat _compfileFile methods as top level calls (i.e., no 'in' part)
      if home_sel
        meth = home_sel
        meth = nil if meth._equal?( :__compileFile )
      end
      if home_sel # if source
  #     # get baseline and file name from comment at end of method's source
  #     lines = source.split("\n").grep(/# method/)
  #     unless lines.empty?
  #       if /line (\d+) .* file (.*)/=~ lines[-1]
  #         baseline = $1.to_i   
  #         file = $2
  #         # baseline and line are both 1-based , so -1 here
  #         lnum = baseline + line - 1 
  #         return [file[0..-2], lnum , meth]
  #       end
  #     end
        if baseline._isFixnum
          lnum = baseline + line - 1 # baseline and line are both 1-based, so -1
        else
          lnum = line
        end
        return [ file, lnum, meth ]
      end
    elsif include_st
      # Process a smalltalk or bridge method stack frame
      if env_id._equal?(1) && is_bridge
        return ["bridge", line, home_sel]
      elsif env_id._equal?(0)
        return ["smalltalk", line, home_sel]
      else
        # exclude env_id > 1 (typically Ruby parser)
      end
    else
      # exclude, not env 1
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

  class_primitive_nobridge 'main', 'rubyMain'

  class_primitive_nobridge '__basic_new', 'rubyBasicNew'

  def self.new(*args, &block)
    unless block_given?
      raise ThreadError, 'no block given'
    end
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


