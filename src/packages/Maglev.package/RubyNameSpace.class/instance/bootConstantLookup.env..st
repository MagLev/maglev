*maglev-runtime
bootConstantLookup: aSym env: env  
   "compileTime resolveConstant, returns an Association or nil"
  | ns |
  ns := self .
  [ ns ~~ nil ] whileTrue:[ | par |
    (ns resolveConstant: aSym) ifNotNil:[: assoc | ^ assoc ].
    ns :=  ns parent .
  ].
  ^ nil

