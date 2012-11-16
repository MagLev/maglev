*maglev-runtime
addAssociation: anAssociation
| key pns |
"Adds the argument to the receiver."
(anAssociation isKindOf: SymbolAssociation) ifFalse:[
    ArgumentTypeError signal:' expected a kindOf SymbolAssocation'
].
anAssociation _objectSecurityPolicy: self objectSecurityPolicy .
self _at: (key := anAssociation key) put: anAssociation .
"callers responsible for replicating store to persistentCopy"

