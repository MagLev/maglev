*maglev-runtime
tailForPrint: aSize
  self size > aSize ifTrue:[ ^ '...', (self last: aSize) ] 
	              ifFalse:[ ^ self ].

