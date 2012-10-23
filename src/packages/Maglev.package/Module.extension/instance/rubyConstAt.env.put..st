*maglev-runtime
rubyConstAt: aName env: envId put: aValue
    "Returns aValue"
  | tns sym pns cst |
  (sym := aName) _isSymbol ifFalse:[ 
    sym _isOneByteString ifFalse:[ ArgumentTypeError signal:'expected a String or Symbol'].
    sym := aName asSymbol .
  ].
  tns := self transientNameSpaceForStore: envId .
  self _incrementCachedConstantsSerialNum .
  tns at: sym transientRuntimePut: aValue .    
  (pns := tns persistentCopy) ifNotNil:[
    (cst := RubyCompilerState current) persistenceMode ifTrue:[
      [ 
        pns at: sym runtimePut: aValue 
      ] on: ArgumentTypeError do:[:ex | | ok |
        cst installingPrims ifTrue:[
          (pns associationAt: sym otherwise: nil ) ifNotNil:[ :assoc|
             assoc isInvariant ifTrue:[ | oldVal |
               (oldVal := assoc _valueNoAction ) == aValue 
                ifTrue:[ ok := true ]
                ifFalse:[
                  [ 
                    oldVal = aValue ifTrue:[ ok := true ] "attempt env 0 first"
                  ] onSynchronous: AbstractException do:[ :ax |
                    [ 
                      (oldVal @ruby1:==: aValue ) ifTrue:[ ok := true ] 
                    ] onSynchronous: AbstractException do:[ :bx | "reattempt in env 0"
                      "ignore"
                    ]
                 ]. 
               ].
             ] 
          ]
        ].
        ok ifNil:[ ex outer ]
      ].
    ].
  ].
  (aValue isBehavior and:[  aValue name == #'']) ifTrue:[ 
     "assignment to a ruby constant has side effect of naming a class 
        which previously had no name."
     aValue setNameSpaceParent: tns  name: sym env: envId .
  ].
  ^ aValue 

