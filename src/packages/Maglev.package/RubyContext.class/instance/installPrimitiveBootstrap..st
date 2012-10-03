*maglev-runtime
installPrimitiveBootstrap: envId
      "ruby_selector_suffix dependent"
   { { Behavior . #primitive      . #installPrimitive:selector: }.
     { Behavior . #primitive_nobridge .  #installPrimitiveNobridge:selector:  }.
     { Behavior . #primitive      . #installPrimitive:  }.
     { Behavior . #primitive_nobridge .  #installPrimitiveNobridge:  }.
     { Behavior . #class_primitive    .   #installClassPrimitive:selector: }.
     { Behavior . #class_primitive_nobridge . #installClassPrimitiveNobridge:selector:  }.
     { Behavior . #class_primitive    .   #installClassPrimitive:  }.
     { Behavior . #class_primitive_nobridge  . #installClassPrimitiveNobridge:  }.
     { Behavior . #primitive_env      .     #installPrimitiveEnv:sel:suffix:  }.
     { Behavior . #primitive_nobridge_env . #installPrimitiveNobridgeEnv:sel:suffix:  }.
     { Behavior . #class_primitive_nobridge_env . #installClassPrimitiveNobridgeEnv:sel:suffix:  }.
     { Behavior . #method_added  . #_method_added: } .
     { Object   . #class  . #class } .
     { Object   . #singleton_method_added  . #_singleton_method_added: } .
   } do:[ :elem | | cls prefix stSel desc rubySel |
       cls := elem at: 1 . prefix := elem at: 2 .  stSel := elem at: 3 .
       rubySel := prefix _asSymbolWithRubySuffix: (stSel numArgs bitShift: 2) .
       cls addRubySelector: rubySel  env: envId 
          smalltalkMethod: (cls compiledMethodAt: stSel)
    ]. 

