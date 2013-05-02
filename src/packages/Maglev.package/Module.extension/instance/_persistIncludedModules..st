*maglev-runtime
_persistIncludedModules: methodsPersistable
  | klass superklass envId |
  "copying included modules in persistent dict"
  envId := 1 . 

  klass := self .
  superklass := (klass rubySuperclass: envId) .
  [
    ((superklass rubyFullName includesSubString: 'MaglevRecord') or: [ superklass class ~~ Module ] ) ifTrue: [
      superklass _setRubyModulePersistentBit: true.
      (superklass objectSecurityPolicy == nil) ifTrue: [ | pcopy |
          pcopy := superklass primaryCopy .
          pcopy ifNotNil: [ pcopy _setPersistable: methodsPersistable ] .
          superklass _setPersistable: methodsPersistable.
          klass persistentRubySuperclass: envId put: superklass .
      ].
      klass := superklass .
      superklass := (klass rubySuperclass: envId) .
    ] ifFalse: [
      superklass := superklass rubySuperclass: envId .
    ].
    superklass ~~ Object and: [ superklass ~~ nil]
  ] whileTrue.
