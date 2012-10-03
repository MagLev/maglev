*maglev-runtime
arity
  "a ruby primitive"
  | n |
  (n := arity) ifNotNil:[  
      n >= 0 ifTrue:[ ^ n ].
  ].
  ^ (self nonBridgeGsMethod: 1"__callerEnvId") rubyArity  .

