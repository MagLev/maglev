as yet unclassified
irArgNodes
  | res itr  |
  res := super irArgNodes "handles all but iterNode"  .
  (itr := iterNode ) ifNotNil:[
	res add: itr irNode .
	^ res .
  ].
  implicitBlockArg ifTrue:[ 
    "walkWithScope: has forced an implicit block arg"
    res add: self irImplicitBlockArg 
  ].
  ^ res

