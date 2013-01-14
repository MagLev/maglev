*maglev-runtime
parent: aTransientNs name: aSymbol
  | pns n |
  (n := name) == aSymbol ifFalse:[ 
    n == #'' ifFalse:[ self error:'parent:name: - inconsistent name' ].
    name := aSymbol .
  ].
  self parent: aTransientNs .

