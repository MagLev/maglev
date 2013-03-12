*maglev-runtime
_setPersistable: methodsPersistable
  "a ruby primitive"
  | envId tns  parTns parPns savePm cst  wasPersistable |
  envId := 1"__callerEnvId" .
  wasPersistable := self _setRubyModulePersistentBit: true .
  wasPersistable ifFalse:[
    tns := self transientNameSpaceForStore: envId .
    ( parTns := tns parent) == tns ifTrue:[  ^ self  "do nothing  when self==Object" ].
    (parPns := parTns persistentCopy) ifNil:[ ArgumentError signal:'parent is not yet persistable' ].
    tns persistentCopy ifNil:[ | pns |
       pns := RubyNameSpace new initializeForModule: self env: envId .
       pns parent: parPns .
       tns persistentCopy: pns .
       self persistentNameSpace: envId put: pns .
    ].
    cst := RubyCompilerState current .
    savePm := cst persistenceMode .
    [ cst persistenceMode: true .
       parTns at: name runtimePut: self .
    ] ensure:[
      cst persistenceMode: savePm
    ]
  ].
 methodsPersistable ifTrue: [
    tmd := self transientMethodDictForStoreEnv: 1.
    pmd := self persistentMethodDictForStoreEnv: 1.
    tmd keysAndValuesDo: [:key :value |
     pmd at: key put: value.
    ].
  ].

