*maglev-runtime
copyAssociationsToPns
  | pns |
  "caller checks persistenceMode"
  (pns := persistCopy) ifNotNil:[
    self associationsDo:[:assoc |  pns addAssociation: assoc ].
  ].

