*maglev-runtime
valueToNumber: aString

  aString isDigitsForInteger ifTrue:[
    ^ Integer fromString: aString
  ] ifFalse:[
    ^ Float fromString: aString
  ]

