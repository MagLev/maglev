# MagLev has protection problem with this code:
#
#

#   $ mruby $pbm
#   #<NoMethodError: NoMethodError: private method `compiled_template_method_remover' for Tilt::Template>`compiled_template_method_remover' called
#   /Users/pmclain/GemStone/dev/pbm.rb:21:in `method_missing'
#   /Users/pmclain/GemStone/dev/pbm.rb:21:in `compile_template_method'
#   /Users/pmclain/GemStone/dev/pbm.rb:16:in `evaluate'
#   /Users/pmclain/GemStone/dev/pbm.rb:32
#   ERROR 2010, NoMethodError: private method `compiled_template_method_remover' for Tilt::Template (NoMethodError)

module Tilt
  class Template
    def evaluate
      compile_template_method
    end
    private
    def compile_template_method
      ObjectSpace.define_finalizer self,
      Template.compiled_template_method_remover
    end

    def self.compiled_template_method_remover
      proc { puts "Proc called"; 767 }
    end
  end
end


t = Tilt::Template.new
t.evaluate
