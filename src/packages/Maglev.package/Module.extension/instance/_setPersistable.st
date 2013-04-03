*maglev-runtime
_setPersistable: methodsPersistable
  "a ruby primitive"
  | envId tns klass2 parTns parPns savePm cst wasPersistable klass superklass moduleKlass |
  envId := 1"__callerEnvId" .
  wasPersistable := self _setRubyModulePersistentBit: true .
  wasPersistable ifFalse: [
    tns := self transientNameSpaceForStore: envId .
    ( parTns := tns parent) == tns ifTrue:[  ^ self  "do nothing  when self==Object" ].
    (parTns ~~ nil and: [(parPns := parTns persistentCopy) == nil]) ifTrue: [
      (parTns myClass _persistable) ifFalse:[
        ArgumentError signal:'parent ', parTns myClass rubyFullName asString ,' is not yet persistable'
      ]
    ].
    tns persistentCopy ifNil:[ | pns |
      pns := RubyNameSpace new initializeForModule: self env: envId .
      pns parent: parPns .
      tns persistentCopy: pns .
      self persistentNameSpace: envId put: pns .
	  methodsPersistable ifTrue: [
	    tns keysAndValuesDo: [:key :value |
		  pns at: key runtimePut: value .
        ].
      ].
    ].
    cst := RubyCompilerState current .
    savePm := cst persistenceMode .
    [ cst persistenceMode: true .
      (parTns ~~ nil) ifTrue: [
        parTns at: name runtimePut: self .
      ]
    ] ensure:[
      cst persistenceMode: savePm
  ].
   methodsPersistable ifTrue: [
      self _persistMethods .
      "copying included modules in persistent dict"
      superklass := self rubySuperclass: envId .
      (superklass == nil or: [superklass == Object]) ifFalse: [
        self persistentRubySuperclass: envId put: superklass .
        superklass _setPersistable: methodsPersistable .
      ].
      "make this class' class persistent for class methods"
      klass := self class .
      (klass == nil) ifFalse: [
        klass _persistMethods .
      ].

      klass := self rubySingletonClassForExtend .
      (klass == nil) ifFalse: [
        superklass := (klass rubySuperclass: envId) .
        (superklass _setRubyModulePersistentBit: true) ifFalse: [
          klass persistentRubySuperclass: envId put: superklass .
          (klass persistentNameSpace: envId) ifNil: [
            klass persistentNameSpace: envId put: (klass nameSpace: envId) .
          ]
        ].
        (self class ~~ Module) ifTrue: [
          [superklass ~~ Object class] whileTrue: [
            klass := superklass.
            superklass := (klass rubySuperclass: envId) .
            "Transcript show: klass name; cr; show: superklass name; cr."
            (superklass _setRubyModulePersistentBit: true) ifFalse: [
              klass persistentRubySuperclass: envId put: superklass .
            ].
          ].
        ] ifFalse: [
          "persist methods defined in module's singleton class"
          self rubySingletonClass _persistMethods .
        ]
      ]

    ].
  ].
