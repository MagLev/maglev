# Extracted from activesupport memoizable.rb
$a = 10
module M1
  module InstanceMethods
    def self.included(base)
      puts "#{self}.included(#{base})"
      $a = 20
    end
    def bar
      puts "courtesy M3"
    end
  end

  def memoize
    class_eval 'puts "#{self.inspect}"; include InstanceMethods '
  end
end

class Foo
  extend M1
end

Foo.memoize
unless $a == 20 ; raise 'error'; end
puts "Done"

