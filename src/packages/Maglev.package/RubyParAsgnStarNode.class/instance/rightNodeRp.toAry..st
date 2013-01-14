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
     ] ifFalse:[ | lsiz rta |
       lsiz := leftList size .
       rhsIsToAry := (rta := (toAry == true  and:[ lsiz > 1]) or:[ aNode isSplatNode] ) .
       ( aNode isSplatNode) ifTrue:[  rightList := { aNode } ]
                ifFalse:[  lst size == 0 ifTrue:[ rightList := { RubyZArrayNode _basicNew }] ].
       toAry == true ifTrue:[ aNode class == RubyRpCallArgs ifTrue:[ rightList := { aNode } ]].
       rightList ifNil:[ rightList := lst ].
       self walkRpList: rightList   .
     ].
  ].
"(SessionTemps current at:#InRP otherwise: false) ifTrue:[ self pause ]."
"GsFile gciLogServer:' rhsIsToAry ' , rhsIsToAry asString ."

