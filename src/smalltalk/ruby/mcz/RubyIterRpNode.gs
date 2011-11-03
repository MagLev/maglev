
set class RubyIterRpNode
category: '*maglev-ast'
classmethod:
s_a: args b: body c: srcOfs
  | res |
  (res := self _basicNew) 
     _varNode: args ; position: srcOfs ;
     bodyNode: body .
  ^ res

%


set class RubyIterRpNode
category: '*maglev-runtime'
classmethod:
s_a: args b: body c: srcOfs d: endOfs
  | res |
  (res := self _basicNew) 
     _varNode: args ; position: srcOfs ;
     bodyNode: body endOfs: endOfs .
  ^ res

%


set class RubyIterRpNode
category: '*maglev-ast'
method:
callNode: aNode
  callNode := aNode

%


set class RubyIterRpNode
category: '*maglev-ast'
method:
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

%


set class RubyIterRpNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:iterRp, ', callNode _inspect, ', ', varNode _inspect, 
		', ', bodyNode _inspect  , $]

%

