*maglev
peekToNonSeparator: table
  | pos coll limit |
  pos := position .
  limit := (coll := itsCollection) size .
  [ pos <= limit ] whileTrue:[ | ch |
    ch := coll at: pos .
    (table at: ch asciiValue) == $N ifTrue:[
       position := pos .
       ^ ch .    "next is not white and not separator"
    ]. 
    pos := pos + 1 .
  ] .
  ^ nil

