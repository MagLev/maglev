*maglev-runtime
rubyGlobalVarAssoc: aSymbol env: envId 
  "returns an Associaton, creating it if needed" 
  | rns assoc val  |
  self == Object ifFalse:[ ArgumentTypeError signal:'global variables only in Object.nameSpace'].
  rns := self transientNameSpaceForStore: envId  .
  (assoc := rns associationAt: aSymbol otherwise: nil) ifNil:[ 
    val := rns initialValueForGlobal: aSymbol.
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    val ifNotNil:[ assoc globalVarValue: val ]. 
    rns addTransientAssociation: assoc .
  ].
  ^ assoc 

