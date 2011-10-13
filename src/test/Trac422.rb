class BlankSlate
  class << self
    def hide(name)
      undef_method(name.to_s)
    end
  end
end

raise "fail before hide" if BlankSlate.instance_methods.grep(/^class$/).size != 1
cl = BlankSlate
BlankSlate.hide("class")
raise "fail after hide" if BlankSlate.instance_methods.grep(/^class$/).size != 0

# more coverage of mixed undef_method and remove_method
class CA
  def ma(a)
    a + 10 
  end
  def mb
  end
  def self.hide(sel)
    undef_method(sel)
  end
  def self.rmm(sel)
    remove_method(sel)
  end
end
class CB < CA
  def ma(a)
    a + 100
  end
  def mb
  end
end

o = CB.new
x = o.ma(5)
unless x == 105 ; raise 'error'; end
CB.rmm('ma')
x = o.ma(5)
unless x == 15 ; raise 'error'; end

CB.hide('ma')
ex = 0
begin
  o.ma(6)
rescue NoMethodError
  ex += 2
end
unless ex == 2 ; raise 'error'; end

begin
  CB.hide('ma')  # expect error , already hidden
rescue NameError
  ex += 2
end
unless ex == 4 ; raise 'error'; end

CB.hide('mb')
begin
  CB.rmm('mb')  # expect error, already undef-ed
rescue NameError
  ex += 2
end
unless ex == 6 ; raise 'error'; end

begin
  CB.hide('mc')  # no such method
rescue NameError
  ex += 2
end
unless ex == 8 ; raise 'error'; end

begin
  CB.rmm('mc')  # no such method
rescue NameError
  ex += 2
end
unless ex == 10 ; raise 'error'; end

class CB
  def ma(a)  # undo the hide
    a + 1000
  end
end
x = o.ma(4) 
unless x == 1004 ; raise 'error'; end
x = o.ma(6) { puts 'foo' }
unless x == 1006 ; raise 'error'; end
puts "Ok"
true


#################### Trac Info
# ID:         422
# Summary:    instance_methods on a class contains extra methods
# Changetime: 2010-02-09 23:28:12+00:00
###

#  Note that in the following even though we have undefined the "class" method it is still shown in the list of instance methods.  Note the second listing from MRI that has fewer methods listed and the undefined method is not in the list.
#  
#  MBP:maglev_test lattam$ maglev-irb
#  >> class BlankSlate
#  .. class << self
#  .. def hide(name)
#  .. undef_method(name.to_s)
#  .. end
#  .. end
#  .. end
#  => #<GsNMethod:0x061ccb01>
#  *> BlankSlate.instance_methods
#  => ["==", "Float", "Integer", "String", "Array", "method", "send", "format", "p", "_isSpecial", "class", "hash", "methods", "open", "pause", "halt", "proc", "load", "fail", "raise", "rand", "clone", "to_s", "method_missing", "exit", "===", "_splat_return_value", "to_a", "_splat_lasgn_value", "_par_asgn_to_ary", "_reraise", "=~", "instance_eval", "eval", "inspect", "_compileFile", "split", "instance_variables", "_instVarAtPut", "_instVarAt", "_isBehavior", "throw", "sprintf", "debugger", "binding", "loop", "include", "global_variables", "catch", "autoload", "_resolve_smalltalk_global", "trap", "_system", "system", "sleep_ms", "_sleep_ms", "sleep", "putc", "printf", "print", "_eval", "_storeRubyVcGlobal", "_eval_with_position", "_getRubyVcGlobal", "is_a?", "_binding_ctx", "caller", "_last_dnu_protection", "autoload?", "_at_exit", "at_exit", "puts", "_stub_warn", "__send__", "kind_of?", "require", "print_line", "frozen?", "freeze", "dup", "nil?", "__id__", "object_id", "_each", "instance
#  *> BlankSlate.hide("class")
#  => BlankSlate
#  *> BlankSlate.instance_method("class")
#  Error, 'no method ''class'' for class ''BlankSlate'''
#  END
#  {:commitResult=>:success}
#  *> BlankSlate.instance_methods
#  => ["==", "Float", "Integer", "String", "Array", "method", "send", "format", "p", "_isSpecial", "class", "hash", "methods", "open", "pause", "halt", "proc", "load", "fail", "raise", "rand", "clone", "to_s", "method_missing", "exit", "===", "_splat_return_value", "to_a", "_splat_lasgn_value", "_par_asgn_to_ary", "_reraise", "=~", "instance_eval", "eval", "inspect", "_compileFile", "split", "instance_variables", "_instVarAtPut", "_instVarAt", "_isBehavior", "throw", "sprintf", "debugger", "binding", "loop", "include", "global_variables", "catch", "autoload", "_resolve_smalltalk_global", "trap", "_system", "system", "sleep_ms", "_sleep_ms", "sleep", "putc", "printf", "print", "_eval", "_storeRubyVcGlobal", "_eval_with_position", "_getRubyVcGlobal", "is_a?", "_binding_ctx", "caller", "_last_dnu_protection", "autoload?", "_at_exit", "at_exit", "puts", "_stub_warn", "__send__", "kind_of?", "require", "print_line", "frozen?", "freeze", "dup", "nil?", "__id__", "object_id", "_each", "instance
#  
#  MBP:maglev_test lattam$ irb
#  >> class BlankSlate
#  >> class << self
#  >> def hide(name)
#  >> undef_method(name.to_s)
#  >> end
#  >> end
#  >> end
#  => nil
#  >> BlankSlate.hide("class")
#  => BlankSlate
#  >> BlankSlate.instance_methods
#  => ["inspect", "clone", "method", "public_methods", "instance_variable_defined?", "equal?", "freeze", "methods", "respond_to?", "dup", "instance_variables", "__id__", "taguri", "object_id", "eql?", "id", "to_yaml", "taguri=", "singleton_methods", "send", "taint", "frozen?", "instance_variable_get", "__send__", "instance_of?", "to_a", "to_yaml_style", "type", "protected_methods", "instance_eval", "==", "display", "===", "instance_variable_set", "to_yaml_properties", "kind_of?", "extend", "to_s", "hash", "tainted?", "=~", "private_methods", "nil?", "untaint", "is_a?"]
#  
#  
#  