*maglev-runtime
rubyIvDefined: aString
  "a ruby primitive"
  | rubyName |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  rubyName := aString asSymbol .
  ^ (self rubyClassInstVarDefined: rubyName env: 1"__callerEnvId" ) ~~ nil
  

