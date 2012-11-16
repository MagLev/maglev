converting
irArgNodes
	^ elseBody 
	     ifNil: [  { thenBody irBlockNodeInline: self  } ] 
	     ifNotNil:[ 
		    thenBody  ifNil: [  { elseBody irBlockNodeInline: self  }  ]
			    ifNotNil: [  { thenBody irBlockNodeInline: self . 
				              elseBody irBlockNodeInline: self  } 
				          ] 
		  ]

