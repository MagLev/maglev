*maglev-ast
signalError: aString

  (self _rbCompileError: aString isWarning: false) ifTrue:[
    "parser primitive active on stack and aString was saved in parser state"
  ] ifFalse:[
    "parser primitive not active, signal directly"
    RubyParseError signal: aString 
  ].

