# MagLev is not properly defining the logger method.
#

# From lib/active_support/core_ext/kernel/singleton_class.rb
module Kernel
  # Returns the object's singleton class.
  def singleton_class
    class << self
      self
    end
  end unless respond_to?(:singleton_class) # exists in 1.9.2

  # # class_eval on an object acts like singleton_class.class_eval.
  # def class_eval(*args, &block)
  #   singleton_class.class_eval(*args, &block)
  # end
end

# From: lib/active_support/core_ext/class/attribute.rb
class Class
  def class_attribute(*attrs)

    puts "---- #{self.name}#class_attribute(#{attrs.inspect})"

    attrs.each do |name|
      class_eval <<-RUBY, __FILE__, __LINE__ + 1
        def #{name}
          puts "In 22"
          singleton_class.logger  # This raises method not found
          raise "Fail A" unless singleton_class.methods.include?("logger")
          22
        end
      RUBY
    end
  end
end

# class LogSubscriber
#   def self.logger
#     puts "redefined logger"
#     10
#   end
# end

class LogSubscriber
  class_attribute :logger

  # class << self
  #   remove_method :logger
  # end

  def self.logger
    puts "redefined logger"
    10
  end
end

result = LogSubscriber.new.logger
p result
raise "Fail B" unless result == 22
true
