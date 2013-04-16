*maglev-runtime
_persistIncludedModules: methodsPersistable
  | superklass envId |
  "copying included modules in persistent dict"
  envId := 1 . 
  superklass := self rubySuperclass: envId .
  (superklass == nil or: [superklass == Object]) ifFalse: [
    self persistentRubySuperclass: envId put: superklass .
    superklass _setPersistable: methodsPersistable .
  ].
  