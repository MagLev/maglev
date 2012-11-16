*maglev-runtime
rubyTransientConst: aName put: aValue block: aBlock env: envId
  | tns pns sym assoc passoc |
  (sym := aName) _isSymbol ifFalse:[ 
    sym _isOneByteString ifFalse:[ ArgumentTypeError signal:'expected a String or Symbol'].
    sym := aName asSymbol .
  ].
  tns := self transientNameSpaceForStore: envId .
  self _incrementCachedConstantsSerialNum .
  assoc := tns _at: sym otherwise: nil .
  (assoc ~~ nil and:[ assoc class == RubyTransientConstantAssociation]) ifTrue:[
      ArgumentError signal:'non-transient constant ', sym, ' already exists'.
  ].
  (pns := tns persistentCopy) ifNotNil:[ passoc := pns _at: sym otherwise: nil ].
  (passoc ~~ nil and:[ passoc class == RubyTransientConstantAssociation]) ifTrue:[
     RubyCompilerState current reloadingPrims ifFalse:[
       ArgumentError signal:'persistent non-transient constant ', sym, ' already exists'
     ].
  ].
  assoc ifNil:[
    (assoc := RubyTransientConstantAssociation new) key: sym  .
    tns addTransientAssociation: assoc .  
  ].
  assoc block: aBlock class: self env: envId ; 
        _value: aValue .
  (pns ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
    passoc ifNil:[ 
      (passoc := RubyTransientConstantAssociation new) key: sym  .
      pns addAssociation: passoc .
    ].
    passoc block: aBlock class: self env: envId .  "do not do _value: "
  ].
  ^ aValue

