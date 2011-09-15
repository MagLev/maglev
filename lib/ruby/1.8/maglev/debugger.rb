require 'maglev/objectlog'

Maglev.persistent do
  module Maglev::Debugger
    ExceptionFrames = [:raise, :"signal:", :"signal", "_rubyReraise"]
    RubyEnvironment = 1

    extend self

    ##
    # Saves an exception to the ObjectLog.
    # This will abort the pending transaction.
    def save_exception(exception)
      if Maglev::System.needs_commit
        warn("Saving exception to ObjectLog, discarding transaction")
      end
      Maglev.abort_transaction
      DebuggerLogEntry.create_continuation_labeled(exception.message)
      res = ObjectLog.to_a.last
      begin
        Maglev.commit_transaction
      rescue Exception
        warn "A transaction error occured trying to save a continuation to the stone"
      end
      res
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
      pkey = Thread.current.object_id
      Maglev::System.session_temp_put(:"#{pkey}", Thread.current)
      client = Thread.start(block, !!reraise, pkey) do |blk, reraise_dup, parent|
        begin
          Thread.current[:result] = blk.call
        rescue Exception => e
          if reraise_dup
            save_exception(e)
            raise e
          else
            Thread.current[:exception] = e
            Maglev::System.session_temp(:"#{parent}").wakeup
            Thread.stop
          end
        end
      end
      client.join unless client.alive? && client.stop? # raises exception if client aborts
      if (result = client[:result]).nil?
        self.pop_exception_handling_frames(client)
        client
      else
        result
      end
    end

    private
    ##
    # Searches method frames included in ExceptionFrames from the top
    # of the stack, and resets the stack to the last ruby frame before
    # that. If no useable frame is found, the stack is not modified.
    def pop_exception_handling_frames(thread)
      m = thread.__method_at(i = 1)
      until ExceptionFrames.include?(m.__name) or i >= thread.__stack_depth
        i += 1
        m = thread.__method_at(i)
      end
      if i < thread.__stack_depth
        until m.__env_id == RubyEnvironment or i >= thread.__stack_depth
          i += 1
          m = thread.__method_at(i)
        end
      end
      thread.__trim_stack_to_level(i) unless i >= thread.__stack_depth
    end
  end
end

