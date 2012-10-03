*maglev-runtime
_parAsgnCopyFrom: anInt
  | sz |
  anInt > (sz := self size) ifTrue:[
	^ { }
  ] ifFalse:[
    ^ self copyFrom: anInt to: sz 
  ]

