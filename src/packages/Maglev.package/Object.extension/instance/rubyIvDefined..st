*maglev-runtime
rubyIvDefined: aString
  "a ruby primitive"
  | sym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  ((aString at: 2) == $_ and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    sym := (aString copyFrom: 6 to: aString size) asSymbol
  ] ifFalse:[
    sym := aString asSymbol
  ].
  ^ self _rubyInstvarDefined: { sym . sym . nil . 0 }

