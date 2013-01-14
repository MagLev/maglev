*maglev-runtime
rightNodeRp:  aNode toAry: toAry
  aNode ifNil:[
     rhsIsToAry := false 
  ] ifNotNil:[ | f s lst |
     f := (lst := aNode list) atOrNil: 1 .
     f ifNotNil:[ s := f rpCallArgsList atOrNil: 1 ]. 
     (s ~~ nil and:[ s isSplatNode]) ifTrue:[
       rhsIsToAry := true .
       rightList := { aNode } .
     ] ifFalse:[ | rta |
       rhsIsToAry := (rta :=  toAry == true  or:[ aNode isSplatNode] ) .
       ( aNode isSplatNode) ifTrue:[  rightList := { aNode } ].
       toAry == true ifTrue:[ aNode class == RubyRpCallArgs ifTrue:[ rightList := { aNode } ]].
       rightList ifNil:[ rightList := lst ].
       self walkRpList: rightList  .
     ].
  ].
"(SessionTemps current at:#InRP otherwise: false) ifTrue:[ self pause ]."
"GsFile gciLogServer:' rhsIsToAry ' , rhsIsToAry asString ."

