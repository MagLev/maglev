*maglev
sexpNextOrNil
  "returns next character, or returns nil if at end of stream .
   Assumes self.isBinary ==false "

  | pos coll |
  pos := position .
  pos <= (coll := itsCollection) size ifFalse:[ ^ nil ].
  position := pos + 1.
^ coll at: pos

