as yet unclassified
determineDynamic
  isColon3 == true ifTrue:[ 
	 ^ ( leftIsDynamic := 1 ) 
  ].
  ^ super determineDynamic

