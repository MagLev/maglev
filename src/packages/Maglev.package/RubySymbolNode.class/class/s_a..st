*maglev-ast
s_a: val
  ^ self _basicNew
     name: (val _isSymbol ifTrue:[ val ] ifFalse:[ val symval ])

