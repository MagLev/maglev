converting
irNode
   isMainProgram == true ifTrue:[ self error:'main program should not be here'].
	^ bodyNode irNode

