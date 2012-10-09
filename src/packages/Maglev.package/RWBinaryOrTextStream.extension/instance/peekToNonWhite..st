*maglev
peekToNonWhite: table
  | pos coll limit |
  pos := position .
  limit := (coll := itsCollection) size .
  [ pos <= limit ] whileTrue:[ | ch |
    ch := coll at: pos .
    (table at: ch asciiValue) ~~ $W ifTrue:[
       position := pos .
       ^ ch .    "next is non-white"
    ]. 
    pos := pos + 1 .
  ] .
  ^ nil

