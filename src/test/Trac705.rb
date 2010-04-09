# Distilled from Rails 3
#
# MagLev is assigning an ExecBlock at: base.called_from = begin...end
#
# It seems the regexp within blocks is necessary to reproduce the error.
class Engine
  class << self
    def called_from
      @called_from
    end
    def called_from=(v)
      @called_from = v
    end

    def inherited(base)
#     puts "=== #{self}.inherited(#{base})"
      base.called_from = begin
         call_stack = caller.map { |p| p.split(':')[0..-2].join(':') }
         File.dirname(call_stack.detect { |p|
            p !~ %r[railties[\w\-\.]*/lib/rails|rack[\w\-\.]*/lib/rack]
         })
      end
    end

    def find_root_with_flag
      root_path = self.called_from
      puts "root_path #{root_path}"  # should be a string
      raise "Fail" unless root_path.kind_of? String
    end
  end
end

class Application < Engine
end

class MyApp < Application
end

Application.find_root_with_flag
true
