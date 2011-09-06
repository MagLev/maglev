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
      frames.select do |frame|
        frame.gsmethod.__env_id == RubyEnv
      end
    end

    def frames
      methods = []
      @thread.__stack_depth.times do |idx|
        methods << [@thread.__method_at(idx + 1), idx + 1]
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
    attr_reader :debug_info, :gsmethod, :index, :thread,
      :method_name, :defining_class, :block_nesting

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

    # Step. The manner of stepping defaults to :over, but :into or the
    # number of steps can be passed as arguments
    def step(symbol = :over)
      raise RuntimeError, "can only step top frame" unless @index === 1
      case symbol
      when :into
        @thread.__step_over_in_frame(0)
      when :over
        @thread.__step_over_in_frame(1)
      when Fixnum
        @thread.__step_over_in_frame(arg)
      end
    end

    def context_eval(str)
      context_object.instance_eval(str)
    end

    def context_object
      @context ||= Context.create_for(self,
                                      debug_info![:self],
                                      debug_info![:context])
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
      if @gsmethod.__source_location
        @source_location = @gsmethod.__source_location.join(":")
      end
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
          context[:'(receiver)'] = ary[1]
          context[:'(class)'] = ary[1].class
          context[:'(self)'] = ary[2]
          { :gsMethod => ary[0],
            :receiver => ary[1],
            :self => ary[2],
            :method => ary[3],
            :stepOffset => ary[4],
            :stepPoints => ary[5],
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

    # Emulates the context of a frame execution
    # Defines accessors to the frame locals and sets the instance variables to
    # point to the original object's values
    class Context
      # Tries to create a duplicate of the receiver. If that is not possible,
      # creates a new instance of self. In any case, a singleton class is added
      # to define accessors to frame local values
      def self.create_for(frame, receiver, context_hash)
        begin
          rcv = receiver.dup
        rescue # Make sure that the receiver can be duplicated
               # (If it can't, 'self' and 'class' won't work)
          rcv = self.new(receiver)
        end
        rcv = self.new(receiver) if receiver === rcv

        receiver.instance_variables do |name|
          rcv.instance_variable_set(name, receiver.instance_variable_get(name))
        end
        context_hash.each do |k,v|
          next if [:"(self)", :"(class)", :"(receiver)"].include? k
          rcv.singleton_class.define_method(:k) { v }
          rcv.singleton_class.define_method(:"k=") do |v|
            frame.thread.__frame_at_temp_named_put(frame.index, k, v)
          end
        end
        rcv
      end

      def initialize(rcv)
        @receiver = rcv
      end

      def respond_to?(method)
        @receiver.respond_to? method
      end

      def method_missing(method, *args, &block)
        @receiver.send(method, *args, &block)
      end
    end

  end
end
