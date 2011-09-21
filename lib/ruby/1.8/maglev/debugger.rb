require 'maglev/objectlog'

Maglev.persistent do
  module Maglev::Debugger
    ExceptionFrames = [:raise, :"signal:", :"signal", "_rubyReraise"]
    RubyEnvironment = 1
    Marker = "run through Maglev::Debugger#debug"

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
    # This will abort any pending transaction.
    def save_exception(exception)
      if Maglev::System.needs_commit
        warn("Saving exception to ObjectLog, discarding transaction")
      end
      Maglev.abort_transaction
      res = DebuggerLogEntry.create_continuation_labeled(exception.message)
      res.continuation[Marker] = true
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
    # @param block a block to run in a new Thread in an Exception handler
    # => the result or exception of the passed block
    def debug(&block)
      raise ArgumentError, "must supply a block to debug" unless block

      debug_thread(Thread.start(block) do |blk|
                     begin
                       blk.call
                     rescue Exception => e
                       log_entry = save_exception(e)
                       debugger_e = DebuggerException.new(e, log_entry)
                       raise debugger_e
                     end
                   end)
    end

    ##
    # Resumes the continuation attached to the passed log_entry
    # @param log_entry A DebuggerLogEntry created through #debug
    # => the result or exception of the continuation
    def debug_log_entry(log_entry)
      unless log_entry.respond_to?(:continuation) &&
          log_entry.continuation != nil &&
          log_entry.continuation[Marker] === true
        raise ArgumentError, "must supply a log entry with a continuation " +
          "that was created during the execution of #debug"
      end

      debug_thread(Thread.start { log_entry.resume_continuation })
    end

    private

    ##
    # Run the passed thread and return it's value. This method is
    # called from #debug_log_entry and #debug and thus we rescue a
    # DebuggerException, as the #debug method catches any other
    # Exception and wraps it in a DebuggerException
    def debug_thread(client)
      begin
        # The next line raises if the Thread raised
        client.value
        # (joiner.respond_to?(:call) ? joiner.call : joiner).value
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
    #
    # After the first run, the thread will be reset to the raise of
    # the DebuggerException (above). Running again will try and reset
    # before that.
    def pop_exception_handling_frames(thread, again = true)
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

      # After the first run, the thread will be reset to the raise of
      # the DebuggerException. Running again to try and reset before that.
      pop_exception_handling_frames(thread, false) if again
    end
  end
end
