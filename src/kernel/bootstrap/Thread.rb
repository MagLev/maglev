# Maps to Smalltalk class GsProcess .  See Globals.rb
class Thread
  # constants are defined in Thread1.rb

  class_primitive_nobridge '_stbacktrace', 'backtraceToLevel:'

  def self._backtrace(includeSt, limit)
    unless limit._isFixnum
      raise ArgumentError, 'limit must be a Fixnum'
    end
    unless limit > 0
      raise ArgumentError, 'limit must be > 0'
    end
    result = []
    maglev_home = ENV['MAGLEV_HOME']
    res_start_ofs = 1
    ststack = _stbacktrace(limit)
    for idx in 0..(ststack.length - 1) do
      file, line, meth = _frame_info(ststack[idx])

      # Kernel methods need to refer to the next app level stack info, not
      # to the src/kernel/* files.  This should almost always be the next
      # frame, but eval and block frames may require searching further.
      # This search is a bit sketchy...
      if file =~ KERNEL_SRC_REGEXP
        file1, line1, meth1 = _find_next_user_info_for(idx+1, ststack)
        file, line = file1, line1 if file1  # don't replace meth
      end

      meth = ":in `#{meth}'" unless meth.nil?
      result << "#{file}:#{line}#{meth}" if file
    end
    result[(res_start_ofs+1)..-1]
  end

  # If the current stack frame represents kernel source
  # ($MAGLEV_HOME/src/kernel/*), then this frame should be reported from
  # the calling frame (if it is a user frame).  This mimics how MRI reports
  # frames representing C code.
  def self._find_next_user_info_for(start, stack)
    for idx in (start..stack.length-1) do
      file, line, meth = _frame_info(stack[idx])
      return [file, line, meth] if file and file !~ KERNEL_SRC_REGEXP
    end
    return nil  # failed
  end

  # Return an array of [file_name, line_number, method_name] for the stack
  # frame for ruby stack frames.  If include_st is true, then also return
  # the same information for smalltalk frames.  Ignores env 2.
  def self._frame_info(stack_frame, include_st=false)
    where, line, source = stack_frame

    if /.*>> (.*?):*\*?&? \(envId 1\)/ =~ where
      # Process a ruby stack frame.
      # Treat _compfileFile methods as top level calls (i.e., no 'in' part)
      meth = $1
      meth = nil if meth =~ /_compileFile/
      if source
        lines = source.split("\n").grep(/# method/)
        unless lines.empty?
          if /line (\d+) .* file (.*)/=~ lines[-1]
            baseline = $1.to_i
            file = $2
            return [file[0..-2], baseline + line, meth]
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
    private :_find_next_user_info_for, :_frame_info
  end

  primitive_nobridge 'inspect', '_rubyInspect'

  primitive_nobridge '_join_group', '_joinGroup:'

  def self.abort_on_exception
    false
  end

  # MNI self.abort_on_exception=

  def self.abort_on_exception=(bool)
    _stub_warn("Thread.abort_on_exception=: Does nothing")
  end

  # CriticalMutex defined in Thread1.rb

  def self.critical
    CriticalMutex.locked?
  end

  def self.critical=(bool)
    mutex = CriticalMutex
    locked = mutex.locked?
    if bool
      if locked
        return true
      else
        return mutex.try_lock
      end
    else
      if locked
        mutex.unlock
      end
      return false
    end
  end

  class_primitive_nobridge 'current', '_current'

  class_primitive_nobridge 'exit', 'exit'

  primitive_nobridge '_start*&', 'rubyStart:block:'
  class_primitive_nobridge '_basic_new', 'rubyBasicNew'

  def self.new(*args, &blk)
    thr = self._basic_new
    thr.initialize(*args)
    thr._start(*args, &blk)
  end

  def self.fork(&blk)
    self.start(*[], &blk)
  end

  class_primitive 'start*&', 'rubyStart:block:'

  primitive_nobridge '_terminate', 'terminate'

  def self.kill(thread)
    if thread.is_a(Thread)
      thread._terminate
    else
      raise ArgumentError, 'not a Thread'
    end
  end

  class_primitive_nobridge 'list', 'allProcesses'

  class_primitive_nobridge 'main', 'main'

  class_primitive_nobridge 'pass', 'pass'

  class_primitive_nobridge '_stop', 'stop'

  def self.stop
    self.critical=(false)
    self._stop
  end

  class_primitive_nobridge '_recursion_guard_set', '_recursionGuardSet'

  primitive_nobridge '[]', 'threadDataAt:'

  primitive_nobridge '[]=', 'threadDataAt:put:'

  def abort_on_exception
    false
  end

  # MNI abort_on_exception=

  primitive_nobridge 'alive?' , 'alive'

  primitive_nobridge 'exit', 'exit'
  primitive_nobridge 'kill', 'exit'

  primitive_nobridge 'group' , 'rubyGroup'

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

  primitive_nobridge 'priority', 'rubyPriority'

  primitive_nobridge 'priority=', 'rubyPriority:'

  primitive_nobridge '_kill_ex', 'signalException:'

  def raise(ex_class, message)
    ex = ex_class.exception
    if self.equal?(Thread.current)
      ex.signal(message)
    else
      ex._message=(message)
      self._kill_ex(ex)
    end
  end

  def raise(ex_class, message, *args)
    # TODO  args is callback info not yet implemented
    self.raise(ex_class, message)
  end

  def raise(msg)
    if (msg._isString)
      self.raise(RuntimeError, msg)
    else
      # msg should be a subclass of Exception or
      #  an object that returns a new exception
      ex = msg.exception
      if self.equal?(Thread.current)
        ex.signal
      else
         self._kill_ex(ex)
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

  primitive_nobridge 'stop?', 'rubyStopped'

  primitive_nobridge 'value' , 'joinValue'

  primitive_nobridge 'terminate', 'exit'

  primitive_nobridge 'wakeup', 'rubyResume'


end

