*maglev-runtime
persistentNameSpace: envId
  "result nil if receiver not persistable yet"
  | mds |
  (mds := methDicts) _isArray ifFalse:[
     envId == 0 ifTrue:[ ^ mds ].
     ^ nil
  ].
  ^ mds atOrNil: (envId*4 + 2)

