*maglev-runtime
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

