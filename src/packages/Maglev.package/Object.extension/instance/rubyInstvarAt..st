*maglev-runtime
rubyInstvarAt: aString
  "a ruby primitive. "

  | sym stSym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString _rubyOrdAt: 0) == 64"$@" ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  ((aString _rubyOrdAt: 1) == 95"$_" and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    stSym := (aString copyFrom: 6 to: aString size) asSymbol .
    sym := stSym
  ] ifFalse:[
    sym := aString asSymbol .
    stSym := (aString copyFrom: 2 to: aString size) asSymbol .
  ].
  ^ self _rubyInstvarAt: { stSym . sym . nil . 0 }

