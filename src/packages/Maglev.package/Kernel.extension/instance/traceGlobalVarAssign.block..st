*maglev-runtime
traceGlobalVarAssign: aSymbol block: aBlock
  "a ruby primitive"
  | tmps dict envId |
  envId := 1"__callerEnvId" .
  (aSymbol at:1) == $$ ifFalse:[ ArgumentError signal:'name must begin with ''$'' ' ].
  tmps := SessionTemps current .
  dict := tmps at: #RUBY_traceGlobalVars otherwise: nil .
  aBlock ifNil:[  "untrace_var" | tns |
      tns := Object transientNameSpaceForStore: envId .
      (tns includesKey: aSymbol) ifFalse:[ NameError signal: 'undefined global variable ' , aSymbol ].
      dict ifNil:[ ^ { } ].
      ^ dict removeKey: aSymbol otherwise: { }  .
  ] ifNotNil:[ | trc |
     dict ifNil:[
       tmps at:#RUBY_traceGlobalVars put: ( dict := IdentityKeyValueDictionary new )
     ].
     trc := dict at: aSymbol otherwise: nil.
     trc ifNil:[  dict at: aSymbol put: ( trc := { } ) ].
     trc add: aBlock.
     Module _incrementCachedConstantsSerialNum .
     ^ nil
  ]

