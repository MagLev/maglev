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
