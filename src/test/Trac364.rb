$a = 0
module IRB
  class Context; end

  module ContextExtender
    def self.def_extend_command(cmd_name, load_file, *aliases)
      Context.module_eval %[
        $a = aliases
        puts "A #{aliases.inspect}"
      ]
    end
    def_extend_command(:eval_history, "irb/ext/history.rb", :a, :b)
  end
end

x = $a
unless $a == [ :a , :b ] ; raise 'error' ; end
true
