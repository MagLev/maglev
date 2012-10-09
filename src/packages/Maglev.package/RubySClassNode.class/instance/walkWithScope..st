*maglev-runtime
walkWithScope: aScope
    "used for  class defns of forms      class << anObj  ,   class << ACls  "
  | newScop bdy cst file parentMth |  
  receiverNode walkWithScope: aScope .
  newScop := (scope := RubyLocalStaticScope new ).
  newScop 
    nonInheritingChildOf: aScope ;
    restArg: -1 . 
    "  nameSpace:    not done"
  cst := RubyCompilerState current .
  ( file := cst fileStack topOrNil) ifNotNil:[
     source := file source .  fileName := file fullPath
  ].
  parentMth := cst parentMethodDefOrNil.
  parentMth ifNotNil:[ parentMth setHasBlockArgRef] . "Trac 808"
  (bdy := bodyNode) ifNotNil:[      
     cst pushMethodDef: self .
     [
        bdy walkWithScope: newScop
     ] ensure:[
        cst popMethodDef: self .
     ].
  ].

