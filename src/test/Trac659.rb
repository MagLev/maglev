# From IRB

module IRB
  class Context
  end
end

$w = 5
module IRB
  module ContextExtender
    def self.def_extend_command(cmd_name, *aliases)
      Context.module_eval %[
        for ali in aliases
          $w = ali
        end
      ]
    end
  end
end

IRB::ContextExtender.def_extend_command(:foo, :bar)

# And ensure calling with no aliases also works
IRB::ContextExtender.def_extend_command(:foo)

unless $w.equal?( :bar ) ; raise 'error'; end
true
