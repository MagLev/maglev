#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Logger

    # Bundles different informer instances and sends incoming messages to each.
    # This is the default with Informer as only member.

    class LogHub
      include Logging

      attr_accessor :loggers
      attr_accessor :ignored_tags

      # Takes a list of instances or classes (which will be initialized) and that
      # are added to @loggers. All messages are then sent to each member.

      def initialize(*loggers)
        @loggers = loggers
        @ignored_tags = Set.new
        @loggers.map! do |logger|
          next(nil) if logger == self
          logger.is_a?(Class) ? logger.new : logger
        end
        @loggers.uniq!
        @loggers.compact!
      end

      # integration to Logging

      def log(tag, *args)
        return if @ignored_tags.include?(tag)
        @loggers.each do |logger|
          logger.log(tag, *args)
        end
      end
    end

  end
end
