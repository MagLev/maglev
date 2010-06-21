#-------------  simplified version of Trac749
class C
  def ma(a, &blk)
   yield(a)
  end
  def mb(&blk)
   yield
  end
end
class B < C
  def ma(a)
    super(a) {|o| puts "In BlkA"; $mab = o }
  end
  def mb
    super {|o| 
      puts "In BlkB"
      unless o.equal?(nil) ; raise 'fail'; end
      $mbb = 95 
    }
  end
end
B.new.ma(54)
B.new.mb
unless $mab == 54 ; raise 'fail'; end
unless $mbb == 95 ; raise 'fail'; end

# --------- Trac749 original
# From the rails 3 generator code
class Thor
  module ThorBase

    class << self
      def included(base)
        base.send :extend, ClassMethods
      end
    end

    module ClassMethods
      def start(given_args=:given, config={})
        yield(given_args)
      end
    end

  end

  class Group

    class << self
      def start(x=:x, config={})
        super do |given_args|
          puts "In block"
        end
      end
    end

    include Thor::ThorBase
  end
end

class Base < Thor::Group
end

class MyClass < Base
end
MyClass.start(:quux)


