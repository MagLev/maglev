module IRB
  class Context; end

  module ContextExtender
    def self.def_extend_command(cmd_name, load_file, *aliases)
      Context.module_eval %[
        puts "A #{aliases.inspect}"
        for ali in aliases

        end
      ]
    end
    def_extend_command(:eval_history, "irb/ext/history.rb", :a, :b)
  end
end
