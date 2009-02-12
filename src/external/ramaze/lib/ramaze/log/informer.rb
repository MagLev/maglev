#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Logger

    # A minimal logger for Ramaze, supports files, CLI, colors and some
    # customization.

    class Informer
      include Logging

      attr_accessor :out, :colorize, :log_levels

      # Should Ramaze try to use colors?
      trait :colorize => true

      # parameter for Time.now.strftime
      trait :timestamp => "%Y-%m-%d %H:%M:%S"

      # This is how the final output is arranged.
      trait :format => "[%time] %prefix  %text"

      # Which tag should be in what color
      COLORS = {
        :dev   => :blue,
        :debug => :yellow,
        :info  => :green,
        :warn  => :red,
        :error => :red,
      }

      # Create a new instance of Informer.
      # You can spcify
      #
      # Examples:
      #   Informer.new                    #=> logs to stdout with all levels being
      #                                       shown.
      #   Informer.new($stderr)           #=> same, but to stderr
      #   Informer.new("foo.log")         #=> same, but logs to the file foo.log
      #                                       (or creates it if it doesn't exist yet)
      #   Informer.new($stdout, [:info])  #=> show only #info messages to stdout.

      def initialize(out = $stdout, log_levels = [:debug, :error, :info, :warn])
        @colorize = false

        @out =
          case out
          when STDOUT, :stdout, 'stdout'
            $stdout
          when STDERR, :stderr, 'stderr'
            $stderr
          when IO
            out
          else
            if out.respond_to?(:puts)
              out
            else
              File.open(out.to_s, 'ab+')
            end
          end

        if @out.respond_to?(:tty?) and class_trait[:colorize]
          @colorize = @out.tty?
        end

        @log_levels = log_levels
      end

      # Close the file we log to if it isn't closed already.

      def shutdown
        if @out.respond_to?(:close)
          Log.debug("close, #{@out.inspect}")
          @out.close
        end
      end

      # Integration to Logging.

      def log tag, *messages
        return if closed? || !@log_levels.include?(tag)
        messages.flatten!

        prefix = tag.to_s.upcase.ljust(5)

        if @colorize
          color = COLORS[tag] ||= :white
          prefix.replace prefix.send(color)
        end

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
        @out.respond_to?(:closed?) and @out.closed?
      end
    end

  end
end
