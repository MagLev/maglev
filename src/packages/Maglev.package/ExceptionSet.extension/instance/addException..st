*maglev-runtime
addException: anotherException
  "Ruby support for rescue A, B"

  anotherException _isExceptionClass ifTrue:[
    self at: self size + 1 put: anotherException  . "inline self add:"
  ] ifFalse:[
    anotherException == Object ifTrue:[
      self at: self size + 1 put: Exception 
    ] ifFalse:[| msg |
      msg := 'WARNING, expected to be Object or Exception subclass: ', (anotherException rubyFullName: 1).
      Kernel @ruby1:warn: msg.
    ].
  ].
  ^ self

