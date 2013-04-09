as yet unclassified
addPersistentAssociation: assoc
  "adds assoc by identity to both transient and
  persistent name spaces, per the persistenceMode "
  | pns aSymbol |
  aSymbol := assoc key .
  moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  (self _at: aSymbol otherwise: nil ) ifNotNil:[
    ArgumentTypeError signal:'key already in transient name space'
  ].
  self addAssociation: assoc .
  autoloadCache := assoc .
  ((pns := persistCopy) ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
    (pns  _at: aSymbol otherwise: nil ) ifNotNil:[
      ArgumentTypeError signal:'key already in persistent name space'
    ].
    pns addAssociation: assoc
  ].
  ^ assoc

