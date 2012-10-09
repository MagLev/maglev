*maglev-runtime
rubyInstvarAt: aString env: envId
  "used in implementation of inspect. "
  |  sym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  sym := aString asSymbol .
  ^ self rubyClassInstVarNamed: sym env: envId

