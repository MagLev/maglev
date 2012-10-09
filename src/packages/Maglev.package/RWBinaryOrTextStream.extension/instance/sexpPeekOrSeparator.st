*maglev
sexpPeekOrSeparator
  "returns next character, or returns  Ascii FF if at end of stream .
   Assumes self.isBinary ==false "
  | pos coll |
  pos := position . 
  pos <= (coll := itsCollection) size ifFalse:[ ^ Character withValue: 12"FF" ].
^ coll at: pos

