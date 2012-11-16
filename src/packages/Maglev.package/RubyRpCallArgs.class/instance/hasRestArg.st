as yet unclassified
hasRestArg
  | lst sz | 
  lst := list.
  (sz := lst size) >= 1 ifTrue:[ 
	  ^ ( lst at: sz)  isSplatNode
  ].
  ^ false

