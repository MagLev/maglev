as yet unclassified
breakHandler
  ^ [ :ex | |args | 
	     ( (args := ex gsArguments) at: 1) == 0 ifTrue:[ 
		          ex return: (args at: 2) "terminate the while loop"
		   ] ifFalse:[ ex pass ]
	 ]

