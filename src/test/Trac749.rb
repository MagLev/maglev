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
