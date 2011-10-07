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

#################### Trac Info
# ID:         888
# Summary:    Nice to capture file/line number for all *eval() calls.
# Changetime: 2011-04-12 20:54:33+00:00
###

#  Right now, the file and line information for many/most *eval() calls is "(eval) 2".  It would be nice if we could capture the actual file and line number for the eval.  This would make the WebTools code viewer more useful
#  
#  See also Trac #878
#  
#  From Trac 878:
#  
#  {{{
#  A nice enhancement is to capture the file and line number information
#  for all sends of instance_eval, module_eval and eval, and store that info 
#  with the method. That way, we can quickly find where methods are 
#  getting defined, even if the calling code does not pass file and line 
#  info to eval*().
#  }}}
#  