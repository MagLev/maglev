require 'maglev/objectlog'

module Maglev::Debugger
  ExceptionFrames = [:raise, :"signal:", :"signal", "_rubyReraise"]
  RubyEnv = 1

  extend self

  ##
  # Return the ObjectLog wrapper module
  def object_log
    ObjectLog
  end

  ##
  # Saves an exception to the ObjectLog.
  # This will abort the pending transaction.
  def save_exception(exception)
    if Maglev.needs_commit
      warn("Saving exception to ObjectLog, discarding transaction")
    end
    Maglev.abort_transaction
    DebuggerLogEntry.create_continuation_labeled(exception.message)
    Maglev.commit_transaction
  end

  ##
  # Calls the passed block in a new Thread.
  #
  # If the argument is true, any exception will be saved to the
  # ObjectLog and re-raised. If the argument is false (default),
  # any exception will wake the parent and suspend the failed thread
  # for inspection.
  # => the suspended thread and the exception that stopped it, if any
  # => result of the passed block, if the thread finished
  # => raises Exception, if any
  def debug(reraise = false, &block)
    raise ArgumentError, "must supply a block to debug" unless block

    client = Thread.start(Thread.current, block) do |parent_thread, blk|
      Thread.pass
      begin
        Thread.current[:result] = blk.call
      rescue Exception => e
        if reraise
          save_exception(e.message)
          raise e
        else
          result = Process.new(Thread.current)
          result.exception = e
          Thread.current[:result] = result
          parent_thread.wakeup
          Thread.stop
        end
      end
    end
    client.join # raises exception if client aborted
    if (result = client[:result]).is_a? Process
      result.pop_exception_handling_frames
    end
    result
  end

  class Wrapper; end

  class Process < Wrapper
    attr_accessor :thread, :exception, :timestamp, :label

    def self.new(thread)
      if thread.is_a? ObjectLogEntry
        return ObjectLogError.new(thread) unless self == ObjectLogError
      end
      super(thread)
    end

    def initialize(thread)
      @label = thread.inspect
      @thread = thread
      @timestamp = Time.now
    end

    # Kill process, remove entry from ObjectLog
    def delete
      @thread.exit
    end

    def ruby_frames
      methods = []
      @thread.__stack_depth.times do |idx|
        @thread.__method_at(idx + 1).tap do |o|
          methods << [o, idx + 1] if o.__env_id == RubyEnv
        end
      end
      methods.collect do |method, idx|
        Frame.new(:method => method, :index => idx, :thread => thread)
      end
    end

    ##
    # Searches method frames included in ExceptionFrames from the top
    # of the stack, and resets the stack to the last ruby frame before
    # that. If no useable frame is found, the stack is not modified.
    def pop_exception_handling_frames
      m = @thread.__method_at(i = 1)
      until ExceptionFrames.include?(m.__name) or i >= @thread.__stack_depth
        i += 1
        m = @thread.__method_at(i)
      end
      if i < @thread.__stack_depth
        until m.__env_id == RubyEnv or i >= @thread.__stack_depth
          i += 1
          m = @thread.__method_at(i)
        end
      end
      @thread.__trim_stack_to_level(i) unless i >= @thread.__stack_depth
    end

    def [](key)
      @thread[key]
    end

    def []=(key, value)
      @thread[key] = value
    end

    def to_hash
      { :label => label,
        :process_id => thread.object_id,
        :timestamp => timestamp }
    end
  end

  class ObjectLogError < Process
    def initialize(error)
      super(error)
      @log_entry = error
      @label = error.label
      @timestamp = error.timestamp
      @thread = error.continuation
    end

    def delete
      if Maglev::System.needs_commit
        raise Exception, "Abort would loose data. Commit your data and try again"
      end
      Maglev.abort_transaction
      ObjectLogEntry.object_log.delete(@log_entry)
      Maglev.commit_transaction
    end
  end

  class Frame < Wrapper
    attr_reader :debug_info

    def initialize(params)
      @gsmethod = params[:method]
      @thread = params[:thread]
      @index = params[:index]
      initialize_frame_info
    end

    # Pop frame off the stack, effectively restarting it
    def delete
      @thread.__trim_stack_to_level(@index)
    end

    def context_eval(str)
      raise NotImplementedError, "Do-it on frame not implemented"
    end

    def initialize_frame_info
      label = @gsmethod.__name
      @block_nesting = 0
      m = @gsmethod
      while label.nil?
        m = m.__home_method
        label = m.__name
        @block_nesting += 1
      end
      @defining_class = m.__in_class
      @class = @gsmethod.__in_class
      @method_name = label
      @source_location = @gsmethod.__source_location.join(":")
    end

    def debug_info!
      @debug_info ||=
        begin
          ary = @thread.__gsi_debugger_detailed_report_at(@index)
          context = {}
          # First the named temps, matched to values
          ary[6].each_with_index {|name,idx| context[name] = ary[7][idx] }
          # Then the remaining scope values with temporary names
          ary[7][ary[6].size..-1].each_with_index {|v,i| context[:'t#{i}'] = v}
          context[:'(__receiver__)'] = ary[1]
          context[:'(__class__)'] = ary[1].class
          context[:'(__self__)'] = ary[2]
          { :gsMethod => ary[0],
            :receiver => ary[1],
            :self => ary[2],
            :method => ary[3],
            :stepOffset => (ary[4] ? ary[5][ary[4]] : nil),
            :source => ary[8],
            :context => context }
        end
    end

    def to_hash
      { :method_name => @method_name,
        :class => @class,
        :index => @index,
        :defining_class => @defining_class,
        :source_location => @source_location,
        :block_nesting => @block_nesting,
        :debug_info => @debug_info }
    end
  end
end
