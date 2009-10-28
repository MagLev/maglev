# Extracted from activesupport memoizable.rb
module M1
  module InstanceMethods
    def self.included(base)
      puts "#{self}.included(#{base})"
    end
    def bar
      puts "courtesy M3"
    end
  end

  def memoize
    class_eval "include InstanceMethods"
  end
end

class Foo
  extend M1
end

Foo.memoize


