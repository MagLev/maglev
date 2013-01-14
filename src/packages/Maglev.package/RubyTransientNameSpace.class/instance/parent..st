*maglev-runtime
parent: aNameSpace
  | pns par |
  (par := parent) ifNotNil:[ 
     par ==  aNameSpace ifFalse:[ 
       self error:'module transientNameSpace already has a parent'.
     ]
  ].
  "can't assert that Object's name space has parent == nil   because
      parent is installed before myClass when parsing.  Trac 607" 
  parent := aNameSpace .  
  (pns := persistCopy) ifNotNil:[ 
    RubyCompilerState current persistenceMode ifTrue:[
      pns parent: (aNameSpace ifNotNil:[ aNameSpace persistentCopy ])
    ]
  ]

