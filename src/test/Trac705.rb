# Distilled from Rails 3
#
# MagLev is assigning an ExecBlock at: base.called_from = begin...end
#
# It seems the regexp within blocks is necessary to reproduce the error.
class Engine
  class << self
    attr_accessor :called_from
    # def called_from=(value)
    #   @called_from = value
    # end
    # def called_from
    #   @called_from
    # end

    def abstract_railtie?
      false
    end

    def inherited(base)
      puts "=== #{self}.inherited(#{base})"
      base.called_from = begin
        # Remove the line number from backtraces making sure we don't leave anything behind
        call_stack = caller.map { |p| p.split(':')[0..-2].join(':') }
        File.dirname(call_stack.detect { |p|
           p !~ %r[railties[\w\-\.]*/lib/rails|rack[\w\-\.]*/lib/rack]
        })
      end
    end

    def find_root_with_flag
      root_path = self.called_from
      p root_path  # should be a string
      raise "Fail" unless root_path.kind_of? String
    end
  end
end

class Application < Engine
end

class MyApp < Application
end

Application.find_root_with_flag
