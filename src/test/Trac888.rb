require 'maglev/method_source'

class F
  def ma
  end
  attr_writer :foo
  def mb
  end
end

# p F.instance_methods(false)

 class A
   def self.create_method(name, &block)
     #klass.send(:define_method, name, &block)
     self.send(:define_method, name) { block.call }

     #  SOME COMMENT

   end

   self.create_method(:foo) { puts "hi" }
 end

acls = A
x = A.method_source(:foo)
unless x[0] == "<a define_method >\n { block.call }" ; raise 'fail';end
unless x[2] == 16 ; raise 'fail';end  # line number
y = F.method_source(:foo=)
unless y[1] == '(attr_writer)' ; raise 'fail';end
true

