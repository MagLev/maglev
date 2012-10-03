as yet unclassified
irClassNameLiteralNode
  | assoc |
  (assoc := globAssoc) ifNotNil:[ 
	  ^ assoc key irLiteralNode
  ].
  self error:'unhandled form of a class name'

