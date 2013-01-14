*maglev-runtime
removeKey: aSymbol
  | pns |
  moduleFrozen ifTrue:[ 
      ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  self _removeKey: aSymbol ifAbsent:[] .
       "callee, i.e.  pns removeKey , checks persistenceMode "
  (pns := persistCopy) ifNotNil:[  pns removeKey: aSymbol ].

