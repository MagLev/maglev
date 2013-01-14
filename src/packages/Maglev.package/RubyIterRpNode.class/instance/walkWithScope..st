*maglev-ast
walkWithScope: aScope
  | call  |
  call := callNode .
  call ifNotNil:[ | itr |
    (varNode ~~ nil and:[ varNode ~~ 0]) ifTrue:[ varNode walkIterRpVar  ]. 
    itr := self .
    "AST tree references itr  by identity "
    callNode := nil . 
    call := call asCallNodeForIter . 
    call _becomeMinimalChecks: itr . 
    itr iterNode_forRp: call  .  
    "now walk the call ; walk of call.iter comes here again takes super walk path"
    itr walkWithScope: aScope . 
  ] ifNil:[
    self astAnalyzeArgs .
    super walkWithScope: aScope 
  ]

