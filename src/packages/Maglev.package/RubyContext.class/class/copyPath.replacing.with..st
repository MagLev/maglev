*maglev-runtime
copyPath: anArray replacing: original with: replacement
	"Return a copy of anArray, that shares nothing (both the array and the elements are copied).
	 Convert any leading occurrence of original with replacement.  This is intended for converting
	 between $MAGLEV_HOME and its expansion for load path."
	|path originalEnd|
	originalEnd := original size + 1.
	path := anArray copy .
	1 to: path size do:[:n ||each|
		each := path at: n .
		(each at: 1 equals: original)
			ifTrue:[ each := replacement, (each copyFrom: originalEnd to: each size) ] 
			ifFalse: [ each := each copy ] .
		path at: n put: each .
	 ].
	^ path .

  

