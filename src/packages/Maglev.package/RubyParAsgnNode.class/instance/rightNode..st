as yet unclassified
rightNode:  aNode 
  aNode ifNil:[
	 rhsIsToAry := false 
  ] ifNotNil:[
     aNode isSplatNode ifTrue:[
	   rhsIsToAry := true .
	   rightList := { aNode } .
	 ] ifFalse:[
       rhsIsToAry := aNode isToAryNode .
       rightList := aNode list .
     ].
  ] .

