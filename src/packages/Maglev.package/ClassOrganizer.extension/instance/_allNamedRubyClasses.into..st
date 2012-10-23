*maglev-runtime
_allNamedRubyClasses: aClass into: anIdentitySet
  | result |
  result := IdentitySet new .
  [
    { aClass transientNameSpace: 1 . 
        aClass persistentNameSpace: 1  } do:[:ns |
      ns ifNotNil:[  
        ns keysAndValuesDo:[ :k :v | | child |
          (v isKindOf: Behavior) ifTrue:[ 
              child := v ] ifFalse:[
          (v isKindOf: RubyNameSpace) ifTrue:[ 
             v myClass ifNotNil:[:cls | child := cls ]
          ]].
          (anIdentitySet _addIfAbsent: child) ifTrue:[ 
            child == aClass ifFalse:[
              self _allNamedRubyClasses: child into: anIdentitySet
            ]
          ].
        ]
      ].
    ] 
  ] on: NameError do:[:ex | 
    "handle uninitialized Ruby class variables"
    ex resume: nil 
  ].

