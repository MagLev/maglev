*maglev-runtime
walkWithScope: aScope
    |   nam   |
    implicitDollarTilde walkWithScope: aScope .
    super walkWithScope: aScope . "saves and walks   self argNodes "

    (nam := callName) == #new ifTrue:[
       receiverNode isProcClass ifTrue:[ | mth |
         mth := RubyCompilerState current topMethodDefOrNil.
         mth ifNotNil:[ mth setHasBlockArgRef ].
       ].
    ] ifFalse:[
       (nam at:1) == $c ifTrue:[
         ((nam == #coerce_to or:[nam == #coerce_to_or_nil]) and:[ self rcvr_isTypeClass ]) ifTrue:[
           " allocate a method or block temp in innermost block for use in IR"
           evaluationTmpAssoc := aScope newEvaluationTemp
         ].
       ].
       (RubyAbstractCallNode rcvrNoToProcSelectors includes: nam) ifTrue:[
         receiverNode postWalkForYield .
       ].
    ]

