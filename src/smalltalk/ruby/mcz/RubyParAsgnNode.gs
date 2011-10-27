
set class RubyParAsgnNode
category: '*maglev-ast'
method:
buildBlockArgumentsOn: irBlock

  self _buildBlockArgumentsOn: irBlock .
  leftList size == 1 ifTrue:[  irBlock appendMasgnDummyArg "fix Trac 570"].

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
buildIrLeafsInto: anArray
  | lList lsiz |
  rightList ifNotNil:[ self error:'invalid RHS on ParAsgn as for loop args '].
  lList := leftList .
  1 to: lList size do:[:n | (lList at: n) buildIrLeafsInto: anArray  ].

%


set class RubyParAsgnNode
category: '*maglev-runtime'
method:
emitFirstNRhs: count
  | stms rList tmps |
  stms := { } .    rList := rightList . tmps := evalTemps .
  1 to: count  do:[:n | | ass |
     (ass := GsComAssignmentNode _basicNew) dest: (tmps at: n) leaf  
				source: (rList at: n ) irNode.
     stms add: ( self ir: ass ).
  ].
  ^ stms 

%


set class RubyParAsgnNode
category: '*maglev-runtime'
method:
emitFirstNRhs: count leftSize: lSize 
  | stms rList tmps lim  ass |
  stms := { } .    rList := rightList . tmps := evalTemps .
  lim := lSize >= count ifTrue:[ count -1  "masgn rhs same size or smaller" ] 
                       ifFalse:[ count "masgn rhs bigger" ].
  1 to: lim do:[:n | 
     (ass := GsComAssignmentNode _basicNew) dest: (tmps at: n) leaf  
                 source: (rList at: n ) irNode.
     stms add: ( self ir: ass ).
  ].
  lim == count ifFalse:[ "masgn rhs same or smaller,  Fix Trac629"
     (ass := GsComAssignmentNode _basicNew) dest: (tmps at: count) leaf  
               source: (rList at: count ) irNonSplatNode.
     stms add: ( self ir: ass ).
  ].
  ^ stms 

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
evalTemps
  ^ evalTemps

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
evalTempsSize
  | r |
  ^ (r := rightList) ifNil:[ 1 ] ifNotNil:[ r size ] .

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
hasInnerParAssign
  | lList |
  1 to: (lList := leftList)  size do:[:n |
	  ( lList at: n) isParAssignNode ifTrue:[  ^ true ]
  ].  
  ^ false

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
irAssignmentNode: srcVarNode 
  "for the send from RubyIterNode buildArgumentsOn:  srcVarNode will be a GsComVarLeaf,
   otherwise it should be  a GsComVariableNode "
 
  ^ GsComStatementsNode new 
       list: (self irAssignmentNonStar: srcVarNode size: leftList size )

%


set class RubyParAsgnNode
category: '*maglev-runtime'
method:
irAssignmentNonStar: srcVarNode  size: nonStarCount  
      "ruby_selector_suffix dependent"
  |  send tmpLeaf ass stms lList  |
  srcVarNode class == GsComVarLeaf ifTrue:[  "parallel assign for a complicated ruby block arg"
    tmpLeaf := srcVarNode .  "to_ary coercion not needed"
    stms := { } .  
  ] ifFalse:[  
    tmpLeaf := (evalTemps at: 1) leaf .
    (send := GsComSendNode new)
       rcvr: srcVarNode ;  
       rubySelector: ((rhsToArraySelector ifNil:[ #__par_asgn_to_ary ]) _asSymbolWithRubySuffix: 16r0) . 
    ass := GsComAssignmentNode _basicNew dest: tmpLeaf  source: (self ir: send) .
    stms := { ass } . 
  ].
  lList := leftList .
  1 to: nonStarCount  do:[:n | 
     (send := GsComSendNode new) 
        rcvr: (GsComVariableNode new leaf: tmpLeaf) ;  
        stSelector:  #atOrNil:  ;
        appendArgument: (GsComLiteralNode newInteger: n ).
     ass := ( lList at: n)  irAssignmentNode: ( self ir: send) .
     stms add: ass .
  ].
  ^  stms 

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
irNode
  | rh  |
  rhsToArraySelector := (rh := rightList at: 1)  parAsgnToarySelector .
  ^ rhsIsToAry ifTrue:[ self irAssignmentNode: rh  irNonSplatNode ]
                ifFalse:[ self _irNode ]

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
isParAssignNode
  ^ true

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
isSingleIterArg
  ^ false

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
left: anArray
  leftList := anArray

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
rightNode:  aNode 
  aNode ifNil:[
	 rhsIsToAry := false 
  ] ifNotNil:[
     aNode isSplatNode ifTrue:[
	   rhsIsToAry := true .
	   rightList := { aNode } .
	 ] ifFalse:[
       rhsIsToAry := aNode isToAryNode .
       rightList := aNode list .
     ].
  ] .

%


set class RubyParAsgnNode
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

%


set class RubyParAsgnNode
category: 'as yet unclassified'
method:
setIsBlockArg
  | lList lsiz |
  lList := leftList .
  1 to: lList size do:[:n | (lList at: n) setIsBlockArg ].

%


set class RubyParAsgnNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | lst sz |
  evalTemps := Array new:( sz := self evalTempsSize ) .
  1 to: sz do:[:n | evalTemps at: n put: aScope newEvaluationTemp ]. 

  lst := leftList .
  1 to: lst size do:[:n| (lst at:n) walkWithScope: aScope ].
  lst := rightList .
  1 to: lst size do:[:n| (lst at:n) walkWithScope: aScope ].

%


set class RubyParAsgnNode
category: '*maglev-ast'
method:
_buildBlockArgumentsOn: irBlock
  | lList  |
  rightList ifNotNil:[ self error:'invalid RHS on ParAsgn as block args'].
  lList := leftList .
  1 to: lList size do:[:n | (lList at: n) buildBlockArgumentsOn: irBlock ].
  

%


set class RubyParAsgnNode
category: '*maglev-runtime'
method:
_irNode
  | stms rz lsz lList res rsz tmps  | 
  rsz := rightList size . lList := leftList . lsz := lList size .  tmps := evalTemps . 
  stms := self emitFirstNRhs: rsz leftSize: lsz .
  1 to: lsz do:[:k | | ass val  |
     val :=  k <= rsz ifTrue:[ GsComVariableNode new leaf: (tmps at: k) leaf]
                     ifFalse:[ GsComLiteralNode newNil ].
     ass := ( lList at: k) irAssignmentNode: val .
     stms add: ass .
  ].
  res := GsComArrayBuilderNode new .
  1 to: rsz do:[:m |
     res appendElement: ( GsComVariableNode new leaf: (tmps at: m) leaf ) 
  ].
  stms add: res .
  ^ GsComStatementsNode new list: stms 

%

