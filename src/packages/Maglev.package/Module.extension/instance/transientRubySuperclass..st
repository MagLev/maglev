*maglev-runtime
transientRubySuperclass: envId
  "return superclass for specified env "
  ^ transientMethDicts atOrNil: (envId*4 + 3) 

