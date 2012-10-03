*maglev-ast
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

