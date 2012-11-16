*maglev-runtime
nameSpaceOrNil: envId
  self class == Object ifTrue:[
    ^ Object transientNameSpaceForStore: envId
  ].
  ^ nil

