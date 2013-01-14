*maglev-runtime
at: aSymbol runtimePutInvariant: aValue 
  "used in bootstrap only, receiver should be a peristent name space"
| anAssoc |

anAssoc := self resolveConstant: aSymbol .
anAssoc == nil ifTrue:[ | newAssoc |
    newAssoc := RubySymbolAssociation newWithKey: aSymbol .
    newAssoc _value: aValue .
    newAssoc immediateInvariant .
    self addAssociation: newAssoc.
] ifFalse:[  
  anAssoc _value: aValue; immediateInvariant 
].
^aValue

