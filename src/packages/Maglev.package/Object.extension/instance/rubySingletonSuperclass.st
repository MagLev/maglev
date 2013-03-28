*maglev-runtime
rubySingletonSuperclass
  "Returns the superclass of self's singleton class. Creates
  all singleton classes in the singleton class's hierarchy,
  if necessary."

  self isClass ifTrue: [
    self == Object
      ifTrue: [^ Metaclass3]
      ifFalse: [^ self _superclass rubySingletonClass]]
    ifFalse: [^ self class].

