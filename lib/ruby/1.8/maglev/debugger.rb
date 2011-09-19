require 'maglev/objectlog'

Maglev.persistent do
  module Maglev::Debugger
    ExceptionFrames = [:raise, :"signal:", :"signal", "_rubyReraise"]
    RubyEnvironment = 1

    extend self

    class DebuggerException < Exception
      attr_accessor :cause, :log_entry
      def initialize(cause, log_entry)
        self.cause = cause
        self.log_entry = log_entry
      end
    end

    ##
    # Saves an exception to the ObjectLog.
    # This will abort the pending transaction.
    def save_exception(exception)
      if Maglev::System.needs_commit
        warn("Saving exception to ObjectLog, discarding transaction")
      end
      Maglev.abort_transaction
      res = DebuggerLogEntry.create_continuation_labeled(exception.message)
      begin
        Maglev.commit_transaction
      rescue Exception => e
        warn "Error trying to save a continuation to the stone: #{e.message}"
      end
      res
    end

    ##
    # Calls the passed block in a new Thread. If any Exception occurs,
    # a continuation is saved to the stone for later inspection.
    #
    # => the result or exception of the passed block
    def debug(&block)
      raise ArgumentError, "must supply a block to debug" unless block

      debug_thread(Thread.start(block) do |blk|
                     begin
                       blk.call
                     rescue Exception => e
                       log_entry = save_exception(e)
                       debugger_e = DebuggerException.new(e, log_entry)
                       nil.pause
                       raise debugger_e
                     end
                   end)
    end

    def debug_log_entry(log_entry)
      debug_thread(Thread.start { log_entry.resume_continuation })
    end

    private
    def debug_thread(client)
      begin
        # The next line raises if the Thread raised
        # client.kill if client.stop? && client.alive?
        client.value
      rescue DebuggerException => e
        continuation = e.log_entry.continuation
        self.pop_exception_handling_frames(continuation)
        raise e.cause
      end
    end

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

