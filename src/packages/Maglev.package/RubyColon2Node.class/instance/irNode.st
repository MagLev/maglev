*maglev-runtime
irNode
  globAssoc ifNotNil:[ leftNode ifNil:[ ^ super irNode "defer to colon3"]] . 
  ^ self irC2Node: #( #resolveConst  #dyn_resolveInContext: )

