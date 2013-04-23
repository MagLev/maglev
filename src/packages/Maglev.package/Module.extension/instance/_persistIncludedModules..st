*maglev-runtime
_persistIncludedModules: methodsPersistable
  | superklass envId |
  "copying included modules in persistent dict"
  envId := 1 . 
  superklass := self rubySuperclass: envId .
  (superklass == nil or: [superklass == Object]) ifFalse: [ | pcopy |
    pcopy := superklass primaryCopy .
    pcopy ifNotNil: [ pcopy _setPersistable: methodsPersistable ] .
    superklass _setPersistable: methodsPersistable.
    self persistentRubySuperclass: envId put: superklass .
  ].
  