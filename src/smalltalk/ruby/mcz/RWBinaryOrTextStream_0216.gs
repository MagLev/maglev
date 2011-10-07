
set class RWBinaryOrTextStream
category: '*maglev'
method:
advance: anInt
  position := position + anInt

%


set class RWBinaryOrTextStream
category: '*maglev'
method:
atEquals: aString

  ^ itsCollection at: position equals: aString

%


set class RWBinaryOrTextStream
category: '*maglev'
method:
locationInfo
 
  ^ { position . 
       itsCollection copyFrom: position to: (position + 40 min: itsCollection size)  . 
       itsCollection copyFrom: ( position - 200 max: 1 )
                     to: (position + 200 min: itsCollection size) . 
       itsCollection
     }.

%


set class RWBinaryOrTextStream
category: '*maglev'
method:
peek
  | pos coll |
  pos := position .
  pos <= (coll := itsCollection) size ifFalse:[ ^ nil ]. "inline atEnd"
  ^ coll at: pos   "assumming isBinary is false"

%


set class RWBinaryOrTextStream
category: '*maglev'
method:
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

%


set class RWBinaryOrTextStream
category: '*maglev'
method:
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

%


set class RWBinaryOrTextStream
category: '*maglev'
method:
sexpAdvanceNoCheck

  position := position + 1

%


set class RWBinaryOrTextStream
category: '*maglev'
method:
sexpNextOrNil
  "returns next character, or returns nil if at end of stream .
   Assumes self.isBinary ==false "

  | pos coll |
  pos := position .
  pos <= (coll := itsCollection) size ifFalse:[ ^ nil ].
  position := pos + 1.
^ coll at: pos

%


set class RWBinaryOrTextStream
category: '*maglev'
method:
sexpPeekOrSeparator
  "returns next character, or returns  Ascii FF if at end of stream .
   Assumes self.isBinary ==false "
  | pos coll |
  pos := position . 
  pos <= (coll := itsCollection) size ifFalse:[ ^ Character withValue: 12"FF" ].
^ coll at: pos

%

