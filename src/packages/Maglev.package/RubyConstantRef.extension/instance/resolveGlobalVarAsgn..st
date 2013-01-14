*maglev-runtime
resolveGlobalVarAsgn: aValue
  "called from generated code."
| assoc |
assoc :=  self resolveGlobalVarAssoc .
assoc ifNotNil:[
  assoc rubyGlobalVarValue: aValue .
] ifNil:[ | trc dict rns aSymbol envId |
  envId := 1 "__callerEnvId " .
  aSymbol := self at: 1 .
  dict := SessionTemps current at: #RUBY_traceGlobalVars otherwise: nil .
  dict ifNotNil:[
    (trc := dict at: aSymbol otherwise: nil ) ifNotNil:[
      1 to: trc size do:[ :n | | blk |
        blk := trc at: n .
        blk @ruby1:value: aValue  
      ]
    ].
  ].
  rns := Object transientNameSpaceForStore: envId .
  (assoc := rns associationAt: aSymbol otherwise: nil) ifNil:[
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    rns addTransientAssociation: assoc .
  ].
  assoc rubyGlobalVarValue: aValue .
  trc ifNotNil:[ self setGlobalAssoc_noMarkDirty: assoc ].
].
^ aValue

