*maglev-runtime
rubyGlobalVarDefinedQ: aSymbol
  "called from generated code"
  | rns assoc |
  self == Object ifFalse:[ ArgumentTypeError signal:'global variables only in Object.nameSpace'].
  rns := self transientNameSpaceForStore: 1"__callerEnvId"  .
  (assoc := rns associationAt: aSymbol otherwise: nil) ifNil:[ 
     (rns initialValueForGlobal: aSymbol) ifNil:[ ^ nil ].
  ] ifNotNil:[
    assoc  isDefined ifNil:[ ^ nil ]
  ].
  ^ 'global-variable' copy 

