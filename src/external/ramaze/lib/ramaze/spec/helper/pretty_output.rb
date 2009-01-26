module Bacon
  module PrettyOutput
    NAME = ''

    # store name and run
    def handle_specification(name)
      NAME.replace name
			puts NAME
      yield
			puts
    end

    # Core, yields the requirement and outputs problems
    def handle_requirement(description)
	    print "- #{description}\n"
      error = yield

      unless error.empty?
        if defined?(Ramaze::Logging)
          puts '', " #{NAME} -- #{description} [FAILED]".center(70, '-'), ''
          colors = Ramaze::Logger::Informer::COLORS

          until RamazeLogger.history.empty?
            tag, line = RamazeLogger.history.shift
            out = "%6s | %s" % [tag.to_s, line]
            puts out.send(colors[tag])
          end
        end

        general_error
      end
    end

    # Show nicer output on error
    def general_error
      puts "", ErrorLog
      ErrorLog.scan(/^\s*(.*?):(\d+): #{NAME} - (.*?)$/) do
        puts "#{ENV['EDITOR'] || 'vim'} #$1 +#$2 # #$3"
      end
      ErrorLog.replace ''
    end

    # output summary
    def handle_summary
	    puts
      puts "%d tests, %d assertions, %d failures, %d errors" %
        Counter.values_at(:specifications, :requirements, :failed, :errors)
    end
  end
end

if defined?(Ramaze::Logging)
  module Ramaze
    # Special Logger, stores everything in its history
    class SpecLogger
      include Ramaze::Logging
      include Enumerable

      attr_accessor :history

      # Create new history
      def initialize
        @history = []
      end

      # Yield the history
      def each
        @history.each{|e| yield(e) }
      end

      # general log
      def log(tag, str)
        @history << [tag, str]
      end
    end
  end

  module Bacon::PrettyOutput
    RamazeLogger = Ramaze::SpecLogger.new
    Ramaze::Log.loggers = [RamazeLogger]
  end
end
