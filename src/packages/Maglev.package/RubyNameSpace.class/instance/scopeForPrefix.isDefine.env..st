*maglev-runtime
scopeForPrefix: aPath isDefine: definBool env: eid 
  | pSize scop idx fst |
  
  (pSize := aPath size) > 1 ifTrue:[
    scop := self childScopeAt: (fst := aPath first asSymbol) isDefine: false env: eid .
    idx := 2 .
    scop ifNil:[
		scop := self parentFor: fst .
		scop ifNil:[ scop := Object . idx := 1 ].  
    ].
    [ idx <= (pSize - 1) ] whileTrue:[
       scop := scop childScopeAt: (aPath at: idx) isDefine: definBool env: eid .
       idx := idx + 1.
       scop ifNil:[  ^ nil ].
    ]
  ] ifFalse:[
     scop := self 
  ].
  ^ scop

