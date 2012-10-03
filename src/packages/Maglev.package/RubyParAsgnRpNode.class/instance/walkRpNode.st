*maglev-ast
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

