*maglev-runtime
irDefinedQNode
  | assoc node  |
  self definedQkind == #'global-variable' ifTrue:[ ^ super irDefinedQNode ].

  (assoc := globAssoc) ifNil:[ self error:'colon3 irDefinedQNode missing assoc' ].
  node := assoc irDefinedQNode .
  self ir: node .
  ^ node

