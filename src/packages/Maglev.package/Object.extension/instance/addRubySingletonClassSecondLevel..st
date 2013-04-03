*maglev-runtime
addRubySingletonClassSecondLevel: isSecondLevel

"Insert a new singleton class in the receiver's class hierarchy.
 Returns receiver, or signals an error. The new class is persistable."

  |vCls singletonClassName|
  
  (self _singletonClassAllowedFor: self) ifFalse: [
    ArgumentTypeError signal: 'add singleton class disallowed for ', self name.
    ^ self].

  self isSpecial ifTrue: [
    ArgumentTypeError signal: 'add singleton class disallowed for special objects'.
    ^ self].

  self isCommitted ifTrue: [
    "The new class pointer for this class will not be committed!"
    Kernel @ruby1:warn: 'warning: adding singleton class to committed object'].

  singletonClassName := (self name, ' class') asSymbol.

  "Why did we need envId here?"
  vCls := self _addRubySingletonClass: singletonClassName forSuperClass: (self rubySingletonSuperclassSecondLevel: isSecondLevel).
  
  vCls _isOneByteString
    ifTrue: [ArgumentTypeError signal: 'add singleton class disallowed, ', vCls]
    ifFalse: [vCls destClass: self].

  ^ self

