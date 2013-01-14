*maglev-runtime
walkWithScope: aScope
  | newScop cp bdy cst | 
  (cp := cpath) walkWithScope: aScope .
  cst := RubyCompilerState current .
  scope := (newScop := RubyLocalStaticScope new ). 
  newScop
    requiredArgs: 0;
    restArg: -1;
    setGlobalScopeFor:  cp  parent: aScope env: cst envId .  
  [ (bdy := bodyNode) ifNotNil: [ |  modu  mRef file  |
      ( file := cst fileStack topOrNil) ifNotNil:[ 
         fileName := file fullPath .  source :=  file source 
      ].
      bdy walkWithScope: newScop
    ].
  ] ensure:[
    newScop _nameSpace: nil "avoid commit of tns"
  ]

