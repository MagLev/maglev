*maglev-runtime
locationForName: aName
  | res |
  aName _isSymbol ifFalse:[  self error:'temp/arg name not a symbol'].
  res := self locationForExistingName: aName depth: 0.
  res ifNotNil:[ ^ res ].
 
  ^ self newVarLocation: aName 

