*maglev-runtime
walkWithScope: aScope
      "ruby_selector_suffix dependent"
  | newScop cst file |
  scope := (newScop := RubyLocalStaticScope  new). 
  newScop 
    nonInheritingChildOf: aScope ;
    "no special init for extraArgs. all args appear to be walked before the temps" 
    _nameSpace: aScope nameSpace  .
  cst := RubyCompilerState current.
  cst pushMethodDef: self scope: aScope .
  
  ( file := cst fileStack topOrNil) ifNotNil:[ 
       fileName := file fullPath .  source :=  file source 
  ].
  [ | prefix |
    prefix := nameNode .
    ((prefix _rubyOrdAt: -1) == 108"$l asciiValue"  and:[
          RubyAbstractCallNode evalSelectors includesIdentical: prefix ]) ifTrue:[
      "required to avoid tooManyArgs errors since we synthesize file and line args
       to any call to an eval "
      argsNode ensureStarArg .  "moved to here to Fix Trac 918"
    ].
    argsNode walkWithScope: newScop  .
    bodyNode walkWithScope: newScop .
    hasBlockArgRef ifNil:[ hasBlockArgRef := false ].
  ] ensure:[
    cst popMethodDef: self.
    newScop _nameSpace: nil "avoid commit of tns"
  ].

