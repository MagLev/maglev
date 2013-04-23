*maglev-runtime
_persistSingletonClass
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
    (superklass _setRubyModulePersistentBit: true) ifFalse: [
      (klass persistentNameSpace: envId) ifNil: [
        klass persistentRubySuperclass: envId put: superklass .
      ].
    ].
    klass := superklass .
    superklass := (klass rubySuperclass: envId) .
    klass ~~ Object class
  ] whileTrue.
