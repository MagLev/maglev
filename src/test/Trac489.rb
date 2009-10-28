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
