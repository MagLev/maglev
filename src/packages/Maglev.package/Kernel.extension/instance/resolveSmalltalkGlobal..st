*maglev-runtime
resolveSmalltalkGlobal: aName
  "intented for bootstrap only"
  | assoc |
  assoc := System myUserProfile resolveSymbol: (aName asSymbol) .
  assoc ifNil:[ self error:'Smalltalk global ' , aName , ' not found' ].
  ^ assoc _value

