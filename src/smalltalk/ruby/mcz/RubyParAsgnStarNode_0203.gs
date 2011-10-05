
doit
RubyParAsgnNode subclass: 'RubyParAsgnStarNode'
	instVarNames: #( starSize)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyParAsgnStarNode
removeallmethods
removeallclassmethods

set class RubyParAsgnStarNode
category: '*maglev-runtime'
method:
buildBlockArgumentsOn: irBlock
  self _buildBlockArgumentsOn: irBlock .
  irBlock setLastArgStar

%


set class RubyParAsgnStarNode
category: 'as yet unclassified'
method:
evalTempsSize
   "executed during walkWithScopes phase"
   | rsz r |
   ( r := rightList ) ifNotNil:[
     starSize :=  (rsz := r size) - (leftList size - 1) max: 0 .
     ^ rsz - starSize + 1 
   ] ifNil:[
      "starSize left as nil" 
     ^ 1   "for irAssignmentNode:"
   ]

%


set class RubyParAsgnStarNode
category: '*maglev-runtime'
method:
irAssignmentNode: srcVarNode 
    "see documentation about srcVarNode in superclass"
  |  send  ass stms lList lsiz |
  lList := leftList . lsiz := lList size .

  stms := self irAssignmentNonStar: srcVarNode size: lsiz - 1 .
  (send := GsComSendNode new)
      rcvr: (GsComVariableNode new leaf: (evalTemps at:1) leaf );  
      stSelector:  #_parAsgnCopyFrom:  ;
      appendArgument: (GsComLiteralNode newInteger: lsiz) .
  ass := (lList at: lsiz) irAssignmentNode: (self ir: send) .
  stms add: ass .

  ^ GsComStatementsNode new list: stms 

%


set class RubyParAsgnStarNode
category: '*maglev-runtime'
method:
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

%


set class RubyParAsgnStarNode
category: '*maglev-runtime'
method:
_irNode
  | rList rsz lList lsz  nonStarSz stms n starBld tmps nsRes lastLeaf |
  rList := rightList . rsz := rList size . lList := leftList . lsz := lList size .
       tmps := evalTemps .   nonStarSz  := rsz - starSize  .
  stms := self emitFirstNRhs: nonStarSz  .
  n := nonStarSz  + 1 .
  starBld := GsComArrayBuilderNode new .  
  true ifTrue:[ | lim |   "Fix Trac629"
     [ n < rsz ] whileTrue:[ starBld appendElement: (rList at: n) irNode . n := n + 1 ].
     (n <= rsz ) ifTrue:[ starBld appendElement: (rList at: n) irNonSplatNode . n := n + 1 ].
  ] ifFalse:[
    [ n <= rsz ] whileTrue:[ starBld appendElement: (rList at: n) irNode . n := n + 1 ].
  ].  
  stms add:( self ir:( GsComAssignmentNode _basicNew dest: (lastLeaf := tmps last leaf) 
						source: starBld)).
  1 to: lsz do:[:k | | val ass  |
     val :=  k <= nonStarSz ifTrue:[ GsComVariableNode new leaf: (tmps at: k) leaf ]
              ifFalse:[ k < lsz ifTrue:[ GsComLiteralNode newNil ]
                              ifFalse:[ GsComVariableNode new leaf:  lastLeaf ]].
     ass := (lList at: k) irAssignmentNode: val .
     stms add: ass .
  ].
  nsRes := GsComArrayBuilderNode new .
  1 to: nonStarSz do:[:m |
     nsRes appendElement: ( GsComVariableNode new leaf: (tmps at: m) leaf ) 
  ]. 
  rsz > nonStarSz ifTrue:[ | send |
     (send := GsComSendNode new)
       rcvr: nsRes ;      
       stSelector:  #_rubyAddAll:  ;
       appendArgument: (  GsComVariableNode new leaf: lastLeaf  ) .
     stms add:( self ir: send) .
  ] ifFalse:[     
      stms add: nsRes 
  ].
  ^ GsComStatementsNode new list: stms 

%

