# module_eval/instance_eval do not honor the line number parameter.
class C
end

line_num = 101

C.instance_eval("def classmeth; end", __FILE__, line_num)

m = C.__singleton_class.instance_method(:classmeth)
file, line = m.source_location
raise "Bad line number for class method #{line} expecting #{line_num}" unless line_num == line

line_num = 202
C.module_eval("def foo; end", __FILE__, line_num)
m = C.instance_method(:foo)
file, line = m.source_location
puts file
puts line
raise "Bad line number for instance method #{line} expecting #{line_num}" unless line_num == line
true
