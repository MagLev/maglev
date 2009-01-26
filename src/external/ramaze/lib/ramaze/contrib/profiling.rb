require "ruby-prof"

module Ramaze
  class Dispatcher
    class ActionProfiler < Action
      def self.call(path)
        if RubyProf.running?
          super
        else
          result = RubyProf.profile { super }
          output = StringIO.new
          printer = RubyProf::FlatPrinter.new(result)
          options = {
            :min_percent => Contrib::Profiling.trait[:min_percent],
            :print_file => false
          }
          printer.print(output, options)
          output.string.split("\n").each do |line|
            Log.info(line)
          end
        end
      end
    end
  end

  module Contrib
    class Profiling
      trait :min_percent => 1

      def self.startup
        Dispatcher::FILTER.delete(Dispatcher::Action)
        Dispatcher::FILTER << Dispatcher::ActionProfiler
      end
    end
  end
end
