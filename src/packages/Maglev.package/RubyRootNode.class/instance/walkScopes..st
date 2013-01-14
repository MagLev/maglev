*maglev-runtime
walkScopes: topScopN
  |  inBoot cst newScop |
  cst := RubyCompilerState current .
  inBoot := cst installingPrims .
  inBoot ifTrue:[  isMainProgram := false ].
  staticScope := (newScop := RubyLocalStaticScope new).
  newScop
    requiredArgs: 0;
    restArg: -1 .
  (topScopN isKindOf: RubyStaticScope) ifTrue:[  "fix 829"
    newScop enclosingScope:  topScopN 
  ] ifFalse:[
    newScop _nameSpace:  topScopN"a Module" 
  ].
  newScop
    inBootstrap:  inBoot  ;
    clearVariableNames .
  [ bodyNode ifNotNil:[
      cst pushMethodDef: self .
      [
        bodyNode walkWithScope: staticScope
      ] ensure:[
        cst popMethodDef: self
      ].
    ].
  ] ensure:[
    newScop _nameSpace: nil "avoid commit of tns"
  ]

