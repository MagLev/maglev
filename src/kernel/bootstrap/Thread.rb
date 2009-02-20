# Maps to Smalltalk class GsProcess .  See Globals.rb
class Thread

  def self.name
    'Thread'
  end

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
    _stbacktrace(limit).each do |ary|
      where, line, source = ary
      if /(.*) \(envId 1\)/ =~ where
        meth = $1
        if source
          lines = source.split("\n").grep(/# method/)
          unless lines.empty?
            if /line (\d+) .* file (.*)/=~ lines[-1]
              baseline = $1.to_i
              file = $2
              unless file =~ %r{<file name not available>|#{maglev_home}/src}
                result << "#{file[0..-2]}:#{baseline+line}: in '#{meth}'"
              end
            end
          end
        end
      elsif includeSt
        if  /(.*) \(envId 0\)/ =~ where
          meth = $1
          result << "smalltalk:#{line}: in '#{meth}'"
        else
          result << "smalltalk:#{line}: in #{where} " # usually in Executed Code
        end
      end
    end
    result[1..-1]
  end

  primitive_nobridge '_inspect', '_rubyInspect:'

  primitive_nobridge '_join_group', '_joinGroup:'

  def self.abort_on_exception
    false
  end

  # MNI self.abort_on_exception=

  def self.abort_on_exception=(bool)
    _stub_warn("Thread.abort_on_exception=: Does nothing")
  end

  def self.critical
    false  #
  end

  # MNI def self.critical= ; end
  # use of critical= not yet supported, however the
  #    Smalltalk class ProcessorScheduler does define rubyCritical instVar.

  class_primitive_nobridge 'current', '_current'

  class_primitive_nobridge 'exit', 'exit'

  primitive_nobridge '_start*&', 'rubyStart:block:'
  class_primitive_nobridge '_basic_new', '_basicNew'

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

  class_primitive_nobridge 'stop', 'stop'

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

  primitive_nobridge 'wakeup', 'resume'

end

