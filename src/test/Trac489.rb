# I think this is the basic structure of what is goin on in activesupport...

module CoreExtensions
  module Module
    def alias_method_chain(target, feature)
      puts "-- #{self} alias_method_chain(#{target}, #{feature})"
    end
  end
end

class Module
  include CoreExtensions::Module
end

module Layout
  def self.included(base)
#    base.extend(ClassMethods)
    base.class_eval do
      class << self
        alias_method_chain :inherited, :layout
      end
    end
  end
end

class MyLayout
  include Layout
end
MyLayout.alias_method_chain(:foo, :bar)
#################### Trac Info
# ID:         489
# Summary:    The class of an opened metaclass must respond to ruby methods defined on Class and Method
# Changetime: 2009-11-03 21:29:14+00:00
###

#  As exemplified by the below code frame.  Ruby code expects to be able to invoke methods defined on Class and Module when the receiver is the class of a class.
#  
#  68 [] in  virtualLayout >> included:&       (envId 1) @3 line 4
#      self [114835713 sz:4  VariableContext] aVariableContext
#      receiver [114834945 sz:1  ExecBlockN] anExecBlockN
#      self [115077633  Base class]  Base
#  topaz 1> list
#         def self.included(base)
#           base.extend(ClassMethods)
#           base.class_eval do
#             class << self
#   *  ^3                                                                *******
#               alias_method_chain :inherited, :layout
#     
#     # method starts at line 3 of file /Users/lattam/Projects/MagLev/working/maglev-rails_test/vendor/rails/actionpack/lib/action_controller/layout.rb 
#  
#  