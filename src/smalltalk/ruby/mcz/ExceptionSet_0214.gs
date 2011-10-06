
set class ExceptionSet
category: '*maglev-runtime'
method:
addException: anotherException
  "Ruby support for rescue A, B"

  anotherException _isExceptionClass ifTrue:[
    self at: self size + 1 put: anotherException  . "inline self add:"
  ] ifFalse:[
    anotherException == Object ifTrue:[
      self at: self size + 1 put: Exception 
    ] ifFalse:[
      ArgumentTypeError signal: 'expected Object or anException'
    ].
  ].
  ^ self

%

