*maglev-runtime
_abstractCall: rcvrBlock definedQ: selectorSym
    "returns  'method'  if receiver responds to aSymbol, nil otherwise
     flags say don't cache result in code_gen, ruby lookup, env 1"

  | rcvr |
  rcvr := [ rcvrBlock value ] onException: Exception do:[ :ex | ^ nil  ].
  ^ (rcvr _respondsTo: selectorSym private: true flags: 16r00101) 
       ifTrue:[ 'method' copy ] 
  

