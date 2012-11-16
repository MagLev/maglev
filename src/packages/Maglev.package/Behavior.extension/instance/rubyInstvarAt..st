*maglev-runtime
rubyInstvarAt: aString
  "a ruby primitive."
  | envId |
  envId := 1"__callerEnvId" .
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ]. 
  " @_st_ prefix not supported yet for class inst vars "
  ^ self rubyClassInstVarNamed: aString asSymbol  env: envId

