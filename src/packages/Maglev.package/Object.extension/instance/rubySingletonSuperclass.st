*maglev-runtime
rubySingletonSuperclass
  "Returns the superclass of self's singleton class. Creates
  all singleton classes in the singleton class's hierarchy,
  if necessary."

  self isClass ifTrue: [
    self == Object
      ifTrue: [^ Metaclass3]
      ifFalse: [
        self == Module class 
            ifTrue: [
                "We don't want to create 2nd/higher order singleton
                classes of Behavior."
                ^ Object class]
            ifFalse: [^ self _superclass rubySingletonClass]]]
    ifFalse: [^ self class].

