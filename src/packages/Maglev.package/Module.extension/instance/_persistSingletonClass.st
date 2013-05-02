*maglev-runtime
_persistSingletonClass: methodsPersistable
  | singletonClass envId klass superklass |
  envId := 1 .

  "when self is a module, there are no other singleton superclasses obviously
   only need to persist methods then"
  (self class == Module) ifTrue: [
    self rubySingletonClass _persistMethods .
    ^ nil
  ].
  "nothing to do, if no singleton class"
  singletonClass := self rubySingletonClassForExtend .
  (singletonClass == nil) ifTrue: [^ nil ].

  klass := singletonClass .
  superklass := (klass rubySuperclass: envId) .
  [
    ((superklass rubyFullName includesSubString: 'Maglev') or: [superklass class ~~ Module]) ifTrue: [
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
    klass ~~ Object class and: [ superklass ~~ nil]
  ] whileTrue.
