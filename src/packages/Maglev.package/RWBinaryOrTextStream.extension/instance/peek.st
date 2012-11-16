*maglev
peek
  | pos coll |
  pos := position .
  pos <= (coll := itsCollection) size ifFalse:[ ^ nil ]. "inline atEnd"
  ^ coll at: pos   "assumming isBinary is false"

