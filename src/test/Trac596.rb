# Methods defined in Kernel.rb should be module_methods (i.e., private on Object).
# This tests the methods based on data from MRI

##################################################
#     A simple test case
##################################################
raise "Object#test should be private" unless
  Object.public_instance_methods.grep(/^test$/).empty?

raise "Object#test should be private" if
  Object.private_instance_methods.grep(/^test$/).empty?

##################################################
#     Exhaustive test cases
##################################################
# This array was generated from MRI by:
#    Kernel.methods - Object.public_instance_methods
private_methods_on_object_from_kernel = 
  ["select", "global_variables", "private_class_method", "readline", "warn",
   "const_missing", "gsub", "exit!", "public_instance_methods",
   "method_missing", "method_defined?", "included_modules", "exec", "abort",
   "load", "chomp!", "const_get", "print", "eval", "proc", "untrace_var", "srand",
   "Integer", "local_variables", "class_variables", "module_eval", "readlines",
   "raise", "chop", "protected_instance_methods", "getc", "public_method_defined?",
   "system", "at_exit", "const_set", "putc", "require", "set_trace_func", "rand",
   "test", "lambda", "Float", "p", "class_eval", "chomp", "fail",
   "private_instance_methods", "callcc", "sub!", "include?", "syscall",
   "private_method_defined?", "sleep", "iterator?", "catch", "name", "autoload",
   "puts", "`", "<", "<=>", "String", "sprintf", ">", "split", "caller",
   "instance_method", "gsub!", "open", "protected_method_defined?", "const_defined?",
   ">=", "block_given?", "throw", "ancestors", "<=", "public_class_method", "gets",
   "trap", "sub", "loop", "instance_methods", "Array", "class_variable_defined?",
   "fork", "format", "exit", "constants", "printf", "chop!", "trace_var", "scan",
   "autoload?", "binding"] 

failures = []
private_methods_on_object_from_kernel.each do |m|
  mm = Object.public_instance_methods.grep(/^#{m}$/)
  failures << "#{m.inspect} => #{mm.inspect}" unless mm.empty?
end

raise "FAILED: should not be public: #{failures.join("\n")}" unless failures.empty?
#################### Trac Info
# ID:         596
# Summary:    Kernel module functions should be private instance methods on Object, not public
# Changetime: 2011-09-15 20:29:17+00:00
###

#  This bug found in MiniTest
#  
#  The methods documented in the Pick-Axe under Kernel (i.e., everything defined in Kernel.rb) are supposed to be module methods: You can call them in functional form (w/o a receiver) and they become private methods for classes that mix-in the module (i.e., they should be *private* instance methods on Object).  There are some methods documented under Kernel that are *also* documented under Object as instance methods, and these should be public on Object.
#  
#  It appears that all of the methods in Kernel.rb are showing up as public instance methods on Object.  This messes up reflective processing.
#  
#  The MiniTest framework has the following code:
#  
#  {{{
#   methods = public_instance_methods(true).grep(/^test/).map { |m|
#            m.to_s
#          }.sort
#  }}}
#  
#  This is intended to pick up all methods that begin with "test" so that the framework can run through them and invoke them all as test cases.   Kernel#test (which should be a module method), is found as a public instance method on Object, and becomes one of methods found by the code above.
#  
#  To reproduce: run the associated TracXXX file.
#  
#  