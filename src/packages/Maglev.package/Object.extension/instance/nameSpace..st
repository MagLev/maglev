*maglev-runtime
nameSpace: envId
  self class == Object ifTrue:[
    ^ Object transientNameSpaceForStore: envId
  ].
  ArgumentTypeError signal:'left side of :: is not a class/module ' .
  ^ nil

