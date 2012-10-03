as yet unclassified
argLeaf: aNumber
	^ leaves at: aNumber ifAbsentPut:[ | sym |
		sym := #( t1 t2 t3 t4 t5 t6 ) atOrNil: aNumber .
		sym ifNil:[ sym := ('t', aNumber asString) asSymbol ].
		GsComVarLeaf new methodArg: sym argNumber: aNumber
	  ]

