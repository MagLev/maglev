*maglev-runtime
addTransientAssociation: anAssociation
| key |
"Adds the argument to the receiver."
moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
(anAssociation isKindOf: SymbolAssociation) ifFalse:[
        ArgumentTypeError signal:' expected a kindOf SymbolAssocation'
].
self _at: (key := anAssociation key) put: anAssociation .

