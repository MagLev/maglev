(as yet unclassified)
determineDynamic
 "returns  nil, 0, 1, 2 
  representing  typeError, not dynamic, dynamic , not cachable .
        saves the result in leftIsDynamic and dynamicTypeError "
 | val  lft  |
 (lft :=leftNode) ifNotNil:[
	(val := lft determineDynamic)  ifNil:[
	  dynamicTypeError := true .
	  val := 1 .
	] ifNotNil:[
	  dynamicTypeError := false .
	  val < 2 ifTrue:[ val := 1 ].
	].
 ] ifNil:[ 
    val := 0 .
    dynamicTypeError := false .
 ] .
 leftIsDynamic := val .
 ^ val

