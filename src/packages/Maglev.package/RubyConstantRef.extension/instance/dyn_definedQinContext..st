*maglev-runtime
dyn_definedQinContext: anObject
 "called from generated code"
 | idx mySize assoc envId |
    "evaluate  :: terms"   
envId := 1"__callerEnvId" .
anObject ifNil:[ ^ nil ] .
assoc := anObject  rubyConstAssociationAtOrNil: (self at: 1) env: envId .
assoc ifNil:[ ^ nil ].
mySize := self size .   
idx := 2 .
[ idx <= mySize ] whileTrue:[ | val |
  assoc isDefined ifNil:[ ^ nil ]. 
  val := assoc _valueNoAction .
  assoc := val rubyConstAssociationAtOrNil: (self at: idx) env: envId .
  assoc ifNil:[ ^ nil ].
  idx := idx + 1
].
^ assoc definedQ

