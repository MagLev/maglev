*maglev-runtime
_persistSingletonClass: methodsPersistable with: aBlock
  | singletonClass envId klass superklass val |
  envId := 1 .

  "nothing to do, if no singleton class"
  singletonClass := self rubySingletonClassForExtend .
  (singletonClass == nil) ifTrue: [^ nil ].

  klass := singletonClass .
  superklass := (klass rubySuperclass: envId) .
  [
    (((aBlock value: superklass primaryCopy) or: [aBlock value: superklass]) or: [ superklass class ~~ Module ] ) ifTrue: [
      (superklass objectSecurityPolicy == nil) ifTrue: [ | pcopy |
          pcopy := superklass primaryCopy .
          pcopy ifNotNil: [ pcopy _setPersistable: methodsPersistable with: aBlock ] .
          superklass _setPersistable: methodsPersistable with: aBlock.
          klass persistentRubySuperclass: envId put: superklass .
      ].
      klass := superklass .
      superklass := (klass rubySuperclass: envId) .
    ] ifFalse: [
      superklass := superklass rubySuperclass: envId .
    ].
    klass ~~ Object class and: [ superklass ~~ nil]
  ] whileTrue.
