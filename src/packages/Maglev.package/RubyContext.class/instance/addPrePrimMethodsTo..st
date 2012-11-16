*maglev-runtime
addPrePrimMethodsTo: envId
  "add to specified env,
   methods defined in env 1 in file pre_prim_methods.gs  "

{ { Object . 	#singleton_method_added: } . 
   { Behavior . #method_added:  } . 
   { UndefinedObject . 	#__rubyOr:  } . 
   { Boolean . 	#class  } . 
   { Boolean . 	#__rubyAnd:  } . 
   { Boolean . 	#__rubyXor:  } . 
   { Boolean . 	#__rubyOr: } } do:[ :pair | | meth cls sel |
      cls := pair at: 1 .
      sel := pair at: 2 .
      meth :=  cls compiledMethodAt: sel environmentId: 1 .
      cls addRubySelector: sel method: meth env: envId .  
    ]

