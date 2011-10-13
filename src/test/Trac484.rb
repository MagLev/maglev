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

#################### Trac Info
# ID:         484
# Summary:    class_eval not finding constants
# Changetime: 2009-11-03 21:26:34+00:00
###

#  class_eval is not finding constants defined in the enclosing static scopes.  For example
#  
#  module M1
#    module M2
#       module M3
#        ...
#        end
#        def method
#          class_eval "...include M3..."
#        end
#    end
#  end
#  
#  The above should find M3 by searching the enclosing scopes (based on Rails using this idiom all over the place.