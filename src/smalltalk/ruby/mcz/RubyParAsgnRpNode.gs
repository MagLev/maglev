
doit
RubyNode subclass: 'RubyParAsgnRpNode'
	instVarNames: #( firstNode thirdNode toAry
	                  trailingComma)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyParAsgnRpNode
removeallmethods
removeallclassmethods

set class RubyParAsgnRpNode class
category: '*maglev-ast'
method:
s_a: lhs ofs: srcOfs comma: trailingCommaBool
  | res |
  (res := self _basicNew) firstNode: lhs; position: srcOfs ;
	trailingComma: trailingCommaBool .
  ^ res

%


set class RubyParAsgnRpNode
category: '*maglev-ast'
method:
append_mrhs: val
  thirdNode ifNil:[ | v_cls |
    v_cls := val class .
    (v_cls == RubyRpCallArgs or:[ v_cls == RubySplatNode ]) ifTrue:[
      thirdNode := val
    ] ifFalse:[
      RubyParserM signalError: 'append_mrhs invalid arg' .
    ]
  ] ifNotNil:[
    RubyParserM signalError: 'append_mrhs rhs already present'.
  ].
  ^ self

%


set class RubyParAsgnRpNode
category: '*maglev-ast'
method:
firstNode: lhs
  firstNode := lhs

%


set class RubyParAsgnRpNode
category: '*maglev-ast'
method:
masgn_append_arg: val
   thirdNode ifNil:[ 
     firstNode ifNil:[ " path probably never taken"
       RubyParserM signalError: 'masgn_append_arg lhs is nil' .
     ].
     thirdNode := val .
     toAry := true .
   ] ifNotNil:[
     RubyParserM signalError: ' masgn_append_arg rhs already present'.
   ].
   ^ self

%


set class RubyParAsgnRpNode
category: '*maglev-ast'
method:
trailingComma: trailingCommaBool
  trailingComma := trailingCommaBool

%


set class RubyParAsgnRpNode
category: '*maglev-ast'
method:
walkIterRpVar
  thirdNode ifNil:[ toAry ifNil:[
    | first firstList |
    first := firstNode .  firstList := first list .
    (firstList size == 1 and:[ trailingComma not] )ifTrue:[
      (first := firstList at: 1 ) isSplatNode ifFalse:[ | outer |
        "cope with lack of   blck_var: lhs    term in melbourne grammar"
        "first is usually a RubyLocalAsgnNode."
        outer := self .
        first _becomeMinimalChecks: outer .  
        ^ outer walkRpNode
      ]
    ]
  ]].
  ^ self walkRpNode

%


set class RubyParAsgnRpNode
category: '*maglev-ast'
method:
walkRpNode
  "walk and convert from ParAsgnRpNode to ParAsgnNode "
  | first second third mNode firstList left sz sLine |  
  first := firstNode .  firstList := first list . 
  (sz := firstList size ) >= 2 ifTrue:[  
      ( second := firstList at: sz ) isEmptySplatNode ifTrue:[ 
          second := nil .
          firstList := (firstList copy) size: sz - 1   ].
  ] ifFalse:[ | f fn |  "if first is a non-empty Splat and second nil, force ParAsgnStar"
      ( f := firstList atOrNil: 1) ifNotNil:[ 
         f isSplatNode ifTrue:[ 
           (fn := f node) ifNil:[ first := nil . second := fn .  firstList:= { } .
                                 "empty splat LHS, evaluate RHS for side effects" ]
              ifNotNil:[ firstList := { fn }.  second := f .  "force ParAsgnStar"].
         ].].].
  third := thirdNode  .
  (second == nil or:[ second isSplatNode not or:[ second isEmptySplatNode]]) ifTrue:[ 
      mNode := RubyParAsgnNode _basicNew.  
      left :=  firstList .
  ] ifFalse:[ 
    first ifNil:[ 
      second isEmptySplatNode ifTrue:[ third ifNotNil:[ 
        "empty * on LHS , just emit array builder for RHS"
        third position:  position . 
        third _becomeMinimalChecks: self . 
        ^ self
      ]].
      left := { }
    ] ifNotNil:[     left := firstList    ]. 
    mNode := RubyParAsgnStarNode _basicNew. 
  ].
  mNode left: left ;   rightNodeRp:  third toAry: toAry  ;    position:  position . 
  self walkRpList: left .  
  mNode _becomeMinimalChecks: self .   
  ^ self 

%


set class RubyParAsgnRpNode
category: '*maglev-ast'
method:
walkWithScope: aScope
  | rpNode node cls  |
  rpNode := self .
  cls := self class .
  node := rpNode walkRpNode .  "does conversion and become"
  node class == cls ifTrue:[ self error:'failed to convert ParAsgnRp to ParAsgn'].
  node walkWithScope: aScope .

%


set class RubyParAsgnRpNode
category: '*maglev-runtime'
method:
_inspect
 | res |
  res := '
[:masgnRp bofs ', position _inspect, ', ', firstNode _inspect .
trailingComma ifTrue:[ res addAll: ', <,> ' ].
res addAll:( ', ', thirdNode _inspect , $] ).
^ res

%

