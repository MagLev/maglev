*maglev-runtime
evaluateString: aString binding: aBinding with: vcGlobalsArgs fileName: aFileName 
    lineNumber: aLineNumber  env: envId
  |  evalArgsStk evalArgs |
  "aBinding is a  RubyBinding and takes precedence over self in vcGlobalsSelf"
  ^ [ | bnd |
      bnd := aBinding .
      (evalArgsStk := GsProcess _current _rubyThreadDataAt: 6 ) 
         push: (evalArgs := { bnd . vcGlobalsArgs atOrNil: 4"block_arg" }) .
      self evaluateString: aString  with: vcGlobalsArgs 
          withSelf: bnd  selfObj binding: bnd 
          fileName: aFileName lineNumber: aLineNumber  env: envId 
    ] ensure:[
       evalArgsStk pop: evalArgs 
    ]

