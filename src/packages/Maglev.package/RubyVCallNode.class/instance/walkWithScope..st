*maglev-runtime
walkWithScope: aScope
    implicitDollarTilde walkWithScope: aScope .
    super walkWithScope: aScope.
    (RubyAbstractCallNode rcvrNoToProcSelectors includes: callName) ifTrue:[
      rcvrNode postWalkForYield
    ].
    callName == #new ifTrue:[
       rcvrNode isProcClass ifTrue:[ | mth |
         mth := RubyCompilerState current topMethodDefOrNil.
         mth ifNotNil:[ mth setHasBlockArgRef ].
       ].
    ].

