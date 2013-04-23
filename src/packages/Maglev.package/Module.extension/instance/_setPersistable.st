*maglev-runtime
_setPersistable: methodsPersistable
  "a ruby primitive"
  | envId tns pns parTns parPns savePm cst wasPersistable klass superklass |
  envId := 1"__callerEnvId" .
  wasPersistable := self _setRubyModulePersistentBit: true .
  tns := self transientNameSpaceForStore: envId .
  wasPersistable ifFalse: [
    ( parTns := tns parent) == tns ifTrue:[  ^ self  "do nothing  when self==Object" ].
    (parTns ~~ nil and: [(parPns := parTns persistentCopy) == nil]) ifTrue: [
      (parTns myClass _persistable) ifFalse:[
        ArgumentError signal:'parent ', parTns myClass rubyFullName asString ,' is not yet persistable'
      ]
    ].
    tns persistentCopy ifNil:[
      pns := RubyNameSpace new initializeForModule: self env: envId .
      pns parent: parPns .
      tns persistentCopy: pns .
      self persistentNameSpace: envId put: pns .
      tns keysAndValuesDo: [:key :value |
        pns at: key runtimePut: value .
      ].
    ].
    cst := RubyCompilerState current .
    savePm := cst persistenceMode .
    [ cst persistenceMode: true .
      (parTns ~~ nil) ifTrue: [
        parTns at: name runtimePut: self .
      ]
    ] ensure:[
      cst persistenceMode: savePm
    ].
    methodsPersistable ifTrue: [
      self _persistMethods .

      self _persistIncludedModules: methodsPersistable .

      self _persistClassMethod .

      self _persistSingletonClass .
    ].
  ].

  self _persistAutoloadCache.
