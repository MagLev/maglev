as yet unclassified
_valueNoAction
  isLoaded == true ifTrue:[ ^ value ].
  isLoaded == nil ifTrue:[  "load is active on the stack"
	isDefined == true ifTrue:[ ^ value ].
  ].
  ^ nil

