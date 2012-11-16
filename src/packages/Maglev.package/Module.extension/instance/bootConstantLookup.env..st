*maglev-runtime
bootConstantLookup: aSym env: envId 
  "compile time resolve constant, returns an Association or nil "
  | cls assoc trap |
  cls := self classForConstantLookup: envId forModuleEval: false .
  assoc := (cls transientNameSpaceForStore:  envId) 
                bootConstantLookup: aSym env: envId .
  assoc ifNotNil:[ ^ assoc ].
  self ~~ Object ifTrue:[
    ^ Object bootConstantLookup: aSym env: envId.
  ].
  ^ nil

