as yet unclassified
nextHandler
  ^ [ :ex |  (ex gsArguments at: 1) == 1 ifTrue:[ 
		          ex retry " restart the on:do: for which this block is handler"
		      ] ifFalse:[ ex pass ]
	 ]

