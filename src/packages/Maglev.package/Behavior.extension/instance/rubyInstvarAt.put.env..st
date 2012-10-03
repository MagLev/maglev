*maglev-runtime
rubyInstvarAt: aString put: aValue env: envId
  |  sym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name for ', self rubyName
  ].
  sym := aString asSymbol .
  ^ self rubyClassInstVarNamed: sym put: aValue env: envId

