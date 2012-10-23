*maglev-runtime
rubyRemoveIv: aString
  "a ruby primitive"
  | sym descr |
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
  descr := { sym . sym . nil . 0 } . 
  (self _rubyInstvarDefined: descr) == false ifFalse:[ | val |
      val := self _rubyInstvarAt: descr .
      self _rubyInstvarAt: descr put: _remoteNil privateSize: self rubyPrivateSize .
      ^ val
  ] ifTrue:[
     NameError signal:'instance variable ' , sym , ' not defined'
  ]

