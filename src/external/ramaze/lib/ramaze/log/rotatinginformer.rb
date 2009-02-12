module Ramaze
  
  module Logger

    # A customized logger (based on Informer) that creates multiple log files based on time

    class RotatingInformer
      
      include Logging

      attr_accessor :time_format, :log_levels
      attr_reader :base_dir
      
      # parameter for Time.now.strftime
      trait :timestamp => "%Y-%m-%d %H:%M:%S"

      # This is how the final output is arranged.
      trait :format => "[%time] %prefix  %text"

      # Create a new instance of RotatingInformer.
      #
      # base_dir is the directory where all log files will be stored
      #
      # time_format is the time format used to name the log files.
      # Possible formats are identical to those
      # accepted by Time.strftime
      #
      # log_levelse is an array describing what kind of messages
      # that the log receives. The array may contain
      # any or all of the symbols :debug, :error, :info and/or :warn
      #
      # Examples:
      #   RotatingInformer.new('logs')
      #                                   #=> Creates logs in directory called logs.
      #                                       The generated filenames will be in the
      #                                       form YYYY-MM-DD.log
      #   RotatingInformer.new('logs', '%Y-%m.txt')
      #                                   #=> Creates logs in directory called logs.
      #                                       The generated filenames will be in the
      #                                       form YYYY-MM.txt
      #   RotatingInformer.new('logs', '%Y-%m.txt', [:error])
      #                                   #=> Creates logs in directory called logs.
      #                                       The generated filenames will be in the
      #                                       form YYYY-MM.txt. Only errors will be
      #                                       logged to the files.

      def initialize(base_dir, time_format = '%Y-%m-%d.log', log_levels = [:debug, :error, :info, :warn])
        # Verify and set base directory
        send :base_dir=, base_dir, true
        
        @time_format = time_format
        @log_levels = log_levels
        
        # Keep track of log shutdown (to prevent StackErrors due to recursion)
        @in_shutdown = false
      end

      # Set the base directory for log files
      # 
      # If this method is called with the raise_exception
      # parameter set to true the method will raise an exception
      # if the specified directory does not exist or is unwritable.
      # 
      # If raise_exception is set to false, the method will just
      # silently fail if the specified directory does not exist
      # or is unwritable
      
      def base_dir=(directory, raise_exception = false)
        # Expand directory path
        base_dir = File.expand_path(directory)
        # Verify that directory path exists
        if File.exist?(base_dir)
          # Verify that directory path is a directory
          if File.directory?(base_dir)
            # Verify that directory path is writable
            if File.writable?(base_dir)
              @base_dir = base_dir
            else
              raise Exception.new("#{base_dir} is not writable") if raise_exception
            end
          else
            raise Exception.new("#{base_dir} is not a directory") if raise_exception
          end
        else
          raise Exception.new("#{base_dir} does not exist.") if raise_exception
        end
      end
      
      # Close the file we log to if it isn't closed already.

      def shutdown
        if @out.respond_to?(:close)
          unless @in_shutdown
            @in_shutdown = true
            Log.debug("close, #{@out.inspect}")
            @in_shutdown = false
          end
          @out.close
        end
      end

      # Integration to Logging.

      def log tag, *messages

        return unless @log_levels.include?(tag)

        # Update current log
        update_current_log       

        messages.flatten!

        prefix = tag.to_s.upcase.ljust(5)

        messages.each do |message|
          @out.puts(log_interpolate(prefix, message))
        end

        @out.flush if @out.respond_to?(:flush)
      end

      # Takes the prefix (tag), text and timestamp and applies it to
      # the :format trait.

      def log_interpolate prefix, text, time = timestamp
        message = class_trait[:format].dup

        vars = { '%time' => time, '%prefix' => prefix, '%text' => text }
        vars.each{|from, to| message.gsub!(from, to) }

        message
      end

      # This uses Global.inform_timestamp or a date in the format of
      #   %Y-%m-%d %H:%M:%S
      #   # => "2007-01-19 21:09:32"

      def timestamp
        mask = class_trait[:timestamp]
        Time.now.strftime(mask || "%Y-%m-%d %H:%M:%S")
      end

      # is @out closed?

      def closed?
        @out.respond_to?(:closed?) && @out.closed?
      end

      private

      # Checks whether current filename is still valid.
      # If not, update the current log to point at the new
      # filename

      def update_current_log
        out = File.join(@base_dir, Time.now.strftime(@time_format))
        if @out.nil? || @out.path != out
          # Close old log if necessary
          shutdown unless @out.nil? || closed?

          # Start new log
          @out = File.open(out, 'ab+')
        end
      end
    end

  end
end
