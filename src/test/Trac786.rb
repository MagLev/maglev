# MRI prints: "posts", MagLev prints: nil
#
# From ActiveRecord::Base

class Object
  def singleton_class
    class << self
      self
    end
  end
end

class Base
  class << self
    def define_attr_method(name, value=nil, &block)
      sing = singleton_class
      sing.class_eval <<-eorb, __FILE__, __LINE__ + 1
          if method_defined?(:original_#{name})
            undef :original_#{name}
          end
          alias_method :original_#{name}, :#{name}
        eorb
      if block_given?
        sing.send :define_method, name, &block
      else
        # use eval instead of a block to work around a memory leak in dev
        # mode in fcgi
        sing.class_eval <<-eorb, __FILE__, __LINE__ + 1
            def #{name}; #{value.to_s.inspect}; end
        eorb
      end
    end

    def table_name
      self.table_name = "posts"
    end

    def set_table_name(value = nil, &block)
      @quoted_table_name = nil
      define_attr_method :table_name, value, &block
    end
    alias :table_name= :set_table_name
  end
end

bb = Base
x = bb.table_name
unless x == 'posts' ; raise 'Failed';end
puts "OK"
true
