# class E
# def def_method(object, name, *local_names)
#   method = object.is_a?(Module) ? :module_eval : :instance_eval
   
#   object.send(method,
#               "def #{name}(_haml_locals = {}); puts 'guts'; end",
# #              "def #{name}(_haml_locals = {}); #{precompiled_with_ambles(local_names)}; end",
#               "DEF METHOD", 1)
# end
# end


# E.new.def_method("foo", :render)


Object.new.send(:instance_eval, "def foo(); puts :foo; end")
