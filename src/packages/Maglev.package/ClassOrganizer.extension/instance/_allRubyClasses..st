*maglev-runtime
_allRubyClasses: aClass
  | set |  
  set := IdentitySet new .
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
          (set _addIfAbsent: child) ifTrue:[ 
            child == aClass ifFalse:[
              set addAll: (self _allRubyClasses: child)
            ]
          ].
        ]
      ].
    ] 
  ] on: NameError do:[:ex | 
    "handle uninitialized Ruby class variables"
    ex resume: nil 
  ].
  ^ set

