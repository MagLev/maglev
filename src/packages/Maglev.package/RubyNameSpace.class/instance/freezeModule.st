*maglev-runtime
freezeModule
 
  RubyCompilerState current persistenceMode ifTrue:[
    moduleFrozen := true 
  ].

