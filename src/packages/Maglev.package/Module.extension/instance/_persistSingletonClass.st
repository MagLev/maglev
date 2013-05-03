*maglev-runtime
_persistSingletonClass
  | singletonClass envId superklass |
  
  envId := 1 .

  singletonClass := self rubySingletonClassForExtend .
  (singletonClass == nil) ifFalse: [
    superklass := (singletonClass rubySuperclass: envId) .
    (superklass _setRubyModulePersistentBit: true) ifFalse: [
      singletonClass persistentRubySuperclass: envId put: superklass .
      (singletonClass persistentNameSpace: envId) ifNil: [
        singletonClass persistentNameSpace: envId put: (singletonClass nameSpace: envId) .
      ]
    ].
    (self class ~~ Module) ifTrue: [
      [superklass ~~ Object class] whileTrue: [
        singletonClass := superklass .
        superklass := (singletonClass rubySuperclass: envId) .
        (superklass _setRubyModulePersistentBit: true) ifFalse: [
          singletonClass persistentRubySuperclass: envId put: superklass .
        ].
      ].
    ] ifFalse: [
      "persist methods defined in module's singleton class"
      self rubySingletonClass _persistMethods .
    ]
  ]
