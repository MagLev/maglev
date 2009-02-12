#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # This module provides a basic skeleton for your own loggers to be compatible.
  # The minimal usage is like this:
  #
  #   class MyLogger
  #     include Logging
  #
  #     def log(tag, *args)
  #       p tag => args
  #     end
  #   end

  module Logging

    # Takes the tag (:warn|:debug|:error|:info) and the name of a method to be
    # called upon elements of msgs that don't respond to :to_str
    # Goes on and sends the tag and transformed messages each to the #log method.
    # If you include this module you have to define #log or it will raise.

    def tag_log(tag, meth, *msgs)
      msgs.each do |msg|
        string = (msg.respond_to?(:to_str) ? msg : msg.send(meth))
        log(tag, string)
      end
    end

    # Converts everything given to strings and passes them on with :info

    def info(*objects)
      tag_log(:info, :to_s, *objects)
    end

    # Converts everything given to strings and passes them on with :warn

    def warn(*objects)
      tag_log(:warn, :to_s, *objects)
    end

    # inspects objects if they are no strings. Tag is :debug

    def debug(*objects)
      tag_log(:debug, :inspect, *objects)
    end

    # inspects objects if they are no strings. Tag is :dev

    def dev(*objects)
      tag_log(:dev, :inspect, *objects)
    end

    alias << debug

    # Takes either an Exception or just a String, formats backtraces to be a bit
    # more readable and passes all of this on to tag_log :error

    def error(ex)
      if ex.respond_to?(:exception)
        message = [ex.backtrace].flatten[0..Global.backtrace_size]
        message.map{|m| m.to_s.gsub(/^#{Regexp.escape(Dir.pwd)}/, '.') }
        message.unshift(ex.inspect)
      else
        message = ex.to_s
      end
      tag_log(:error, :to_s, *message)
    end

    # raises

    def log(*args)
      raise "#log should be implemented by an instance including this module (#{self})"
    end

    # nothing

    def shutdown
    end

    # stub for WEBrick

    def debug?
      false
    end
  end

end
