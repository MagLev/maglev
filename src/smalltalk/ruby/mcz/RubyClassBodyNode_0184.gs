
set class RubyClassBodyNode
category: '*maglev-runtime'
method:
classNode
  ^ classNode

%


set class RubyClassBodyNode
category: '*maglev-runtime'
method:
classNode: aNode
  classNode := aNode

%


set class RubyClassBodyNode
category: 'as yet unclassified'
method:
hasSource
  ^ false

%


set class RubyClassBodyNode
category: '*maglev-runtime'
method:
setHasBlockArgRef
  ^ false  "not in an eval"

%


set class RubyClassBodyNode
category: '*maglev-runtime'
method:
walkInEval
  ^ false

%


set class RubyClassBodyNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | newScop kids cst file |
  staticScope := (newScop := RubyLocalStaticScope  new).
  newScop  
    nonInheritingChildOf: aScope ;
    _nameSpace: aScope nameSpace  .
  (cst := RubyCompilerState current) pushMethodDef: self .
  ( file := cst fileStack topOrNil) ifNotNil:[ 
         fileName := file fullPath .  source :=  file source 
  ].
  [ | bdy |
    (bdy := bodyNode) ifNotNil:[ bdy walkWithScope: newScop ].
  ] ensure:[
    cst popMethodDef: self .
    newScop _nameSpace: nil "avoid commit of tns"
  ].

%

