*maglev-runtime
freezeModule
  | pns |
  moduleFrozen := true .
  (pns := persistCopy) ifNotNil:[  
    RubyCompilerState current persistenceMode ifTrue:[
      pns freezeModule 
    ].
  ].

