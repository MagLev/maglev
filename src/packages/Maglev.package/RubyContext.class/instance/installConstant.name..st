*maglev-runtime
installConstant: aName name: stSymbol
  "a Ruby primitive used in bootstrap"
  | stAssoc aKey val envId assoc pns |
  envId := 1"__callerEnvId" .
  stAssoc := System myUserProfile resolveSymbol: ( stSymbol asSymbol) .
  stAssoc ifNil:[ self error:'Smalltalk global ' , stSymbol , ' not found' ].
  aKey := aName asSymbol .
  (aKey at:1) == $$ ifTrue:[ 
      self error: 'installConstant: not to be used for a global variable' 
  ].
  val := stAssoc _value .
  pns := Object persistentNameSpace: envId .
  (assoc := pns resolveConstant: aKey) ifNotNil:[
    assoc _valueNoAction == val ifFalse:[
      Error signal:'Object::', aKey , ' already exists with different identity'
    ]
  ] ifNil:[
    pns at: aKey runtimePutInvariant:  val .
  ].
  RubyNameSpace traceGlobals >= 1 ifTrue:[
    GsFile gciLogServer: 'installGlobal: ', aName , ' : ' , stSymbol .
  ].

