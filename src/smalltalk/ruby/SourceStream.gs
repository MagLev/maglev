!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: SourceStream.gs 25988 2011-06-14 17:55:54Z stever $
!
!=========================================================================
doit
UserGlobals at:#SourceStream ifAbsent:[
 Object subclass: 'SourceStream'
  instVarNames: #(
    position
    input
    inputSize 
    inputAtEnd
    bufPosition
    bufSize
    prevBuf
    buf
    nextBuf
  )  classVars: #( #EofCharacter #MaxSmallSize #NormalBufSize )  
   classInstVars: #()  poolDictionaries: { }
  inDictionary: UserGlobals
]
%
doit
UserGlobals at:#FileSourceStream ifAbsent:[
 SourceStream subclass: 'FileSourceStream'
  instVarNames: #()
  classVars: #()  classInstVars: #()  poolDictionaries: { }
  inDictionary: UserGlobals 
]
%
set class SourceStream
category: 'Private'
classmethod:
_initClassVars

EofCharacter := Character withValue: 16r100 .
MaxSmallSize := 16000 .
NormalBufSize := 4000 .
%
doit
SourceStream  _initClassVars 
%

category: 'Documentation'
classmethod:
comment
" source stream classes for Ruby parser to use.   
  SourceStream is a stream on a String ,
  FileSourceStream is a stream on a GsFile .
 
  These classes provide buffering of large strings or files
  for efficient  next,  position:   etc as used by the Ruby parser .

  hierarchy:
      Object
        SourceStream
          FileSourceStream
"
%


category: 'Instance Creation'
classmethod:
on: aGsFileOrString

| res |
(aGsFileOrString isKindOf: GsFile) ifTrue:[
  res := FileSourceStream new .
] ifFalse:[
  (aGsFileOrString isKindOf: DoubleByteString) ifTrue:[
    self error:'not supported yet.'  
    "change MaxSmallSize logic , change use of basicAt: "
  ].
  res := SourceStream new .
].
res initializeWith: aGsFileOrString .
^ res
%

category: 'Private'
classmethod:
_setBufferSizes_normal: aNorm max: aMax

"for unit testing only !!! "

MaxSmallSize := aMax .
NormalBufSize := aNorm .
%

category: 'Constants'
method:
eofCharacter

^ EofCharacter 
%

category: 'Accessing'
method:
position
  ^ position
%

category: 'Accessing'
method:
atEnd

^ inputAtEnd
%

category: 'Private'
method: 
initialize

position := 1 .
inputAtEnd := false .
prevBuf := nil. 
nextBuf := nil . 
bufPosition := 1 .
%

method: 
normalBufSize
 ^ NormalBufSize
%

category: 'Private'
method: 
initializeWith: aString

self initialize .
input := aString .
inputSize := aString size .
inputSize > MaxSmallSize ifTrue:[  
  bufSize := NormalBufSize .
  buf := aString class new: bufSize .
  self _readNextChunk .
] ifFalse:[
  buf := aString .
  bufSize := inputSize .
].
%

category: 'Accessing'
method:
peek
| startPos res newBufPos |
startPos := position .
res := self next .

(self _currBuf_position: startPos) ifTrue:[
  ^ res
].
self position: startPos .
^ res
%

category: 'Private'
method:
_currBuf_position: aPosition

| newBufPos |
newBufPos := bufPosition - (position - aPosition) .
(newBufPos >= 1 and:[ newBufPos <= bufSize ]) ifTrue:[
  position := aPosition .
  bufPosition := newBufPos .
  ^ true
].
^ false
%

category: 'Positioning'
method:
position: aPosition

"set position of what next will return to specified position.
 returns the receiver."

| thisBufStart  |

(self _currBuf_position: aPosition) ifTrue:[
  ^ self
].
aPosition < position ifTrue:[   "go backwards"
  aPosition < 1 ifTrue:[
    self error:'illegal argument'
  ].
  prevBuf ~~ nil ifTrue:[  | prevBufStart |
    thisBufStart := position - bufPosition + 1 .
    prevBufStart := thisBufStart - prevBuf size .
    aPosition >= prevBufStart ifTrue:[
      nextBuf := buf .
      buf := prevBuf .
      bufSize := buf size .
      prevBuf := nil .
      bufPosition := aPosition - prevBufStart + 1 .
      position := aPosition .
      ^ self
    ].
  ].
] ifFalse:[ "go forwards"
  nextBuf ~~ nil ifTrue:[ | nextBufStart |
    nextBufStart := position + (bufSize - bufPosition + 1) .
    aPosition <= (nextBufStart + nextBuf size - 1) ifTrue:[
      prevBuf := buf .
      buf := nextBuf .
      bufSize := buf size .
      nextBuf := nil .
      bufPosition := aPosition - nextBufStart + 1 .
      position := aPosition .
      ^ self
    ].
  ].
].  
prevBuf := nil .
nextBuf := nil .
inputSize < MaxSmallSize  ifTrue:[ self error:'logic error'].
bufSize := NormalBufSize .
buf size: bufSize .
position := aPosition .
self _seek: position .
self _readNextChunk .
%

category: 'Accessing'
method:
nextMatching: aString

"If the next characters exactly match aString ,
 return true and advance to the next character after aString ,
 otherwise return false and do not change the position .
"
| bPos argSize numAvail halfArg startPosition |
argSize := aString size .
bPos := bufPosition .
numAvail := bufSize - bPos + 1 .
numAvail >= argSize ifTrue:[
  (buf _at: bPos equals: aString ignoreCase: false) ifTrue:[
    bufPosition := bPos + argSize .
    position := position + argSize .
    ^ true 
  ].
  ^ false
].
startPosition := position .
numAvail <= 0 ifTrue:[
  self next == EofCharacter ifTrue:[   "this 'next' reads next buffer"
    self position: startPosition .
    ^ false .
  ].
  self position: startPosition .
  ^ self nextMatching: aString . "try match at start of next buffer's worth"
].

halfArg := aString copyFrom:1 to: numAvail - 1 .
(self nextMatching: halfArg) ifFalse:[
  self position: startPosition .
  ^ false .
].
halfArg := aString copyFrom:(halfArg size + 1) to: argSize .
(self nextMatching: halfArg) ifFalse:[
  self position: startPosition .
  ^ false .
].
^ true
%


category: 'Accessing'
method:
next
"Return the next character.  position is advanced to one past
 the character returned."

| idx bPos |
bPos := bufPosition .
bPos < bufSize ifTrue:[
  bufPosition := bPos + 1 .
  position := position + 1 .
  ^ buf at: bPos .
].
nextBuf ~~ nil ifTrue:[
  prevBuf := buf .
  buf := nextBuf .
  bufSize := buf size .
  nextBuf := nil .
  bufPosition := 1 .
  ^ self next .
].
inputAtEnd ifTrue:[
  ^ EofCharacter .
].
position := position + 1 .
position <= inputSize ifFalse:[
  inputAtEnd := true .
  ^ EofCharacter
].
prevBuf == nil ifTrue:[
  prevBuf := buf.
  buf := buf class new: bufSize .
] ifFalse:[ | tmp |
  tmp := prevBuf .
  prevBuf := buf.
  buf := tmp 
]. 
bufSize := self _readNextChunk .
bufSize <= 0 ifTrue:[ self error:'logic error read past end' ].
bufPosition := 2 .
^ buf at: 1 .
%

category: 'Private'
method:
_readNextChunk
| numToRead |
numToRead := bufSize min: (inputSize - position + 1) .
input copyFrom:position to: (position + numToRead - 1) 
	into: buf startingAt: 1 .
^ numToRead
%

category: 'Private'
method:
_seek: aPosition

"set position of a String input, no action needed "
%



category: 'Accessing'
method:
advanceToEol

"skip forward to the next CR or LF character and return that character,
 or return an EOF character if no Eol found."
| aChar cVal |

[ true ] whileTrue:[
  (aChar := self _currBuf_advanceToEol ) ~~ nil ifTrue:[
    ^ aChar 
  ].
  " we are at last char of current buffer, that char is not a CR or LF"
  aChar := self next .  "advance to first char of next buffer"
  aChar == EofCharacter ifTrue:[ ^ aChar ].
  cVal := aChar asciiValue .
  cVal <= 13 ifTrue:[
    (cVal == 10 or:[ cVal == 13]) ifTrue:[ ^ aChar ]
  ].
]
%

category: 'Private'
method:
_currBuf_advanceToEol
"advance to the next CR or LF within current buffer.
 return that Eolcharacter, or nil if no Eol found ."
 
| b bPosStart bSize |
b := buf .
bPosStart := bufPosition .
bSize := bufSize .

bPosStart to: bSize do:[:j | | cVal |
  cVal := b basicAt: j  .
  cVal <= 13 ifTrue:[
    (cVal == 10 or:[ cVal == 13]) ifTrue:[
      bufPosition := j .
      position := position + (j - bPosStart) .
      ^ Character withValue: cVal .
    ].
  ].
].
bufPosition := bSize .
position := position + ( bSize - bPosStart ) .
^ nil
%

category: 'Accessing'
method:
advanceTo_columnOneEqualsEnd

"skip forward to the next '=end' in column 1 or to EOF .
 return the number of EOL's crossed. 

 resulting position is such that  next  is  $= of the '=end' or EOF"
| eof aChar cr lf eolCount |

eof := EofCharacter .
cr := Character cr .
lf := Character lf .
eolCount := 0 .
[ true ] whileTrue:[
  aChar := self advanceToEol .
  aChar == eof ifTrue:[ ^ eolCount ].
  eolCount := eolCount + 1 .
  [ aChar == lf or:[aChar == cr]] whileTrue:[
    aChar := self next .
    aChar == eof ifTrue:[ ^ eolCount ].
  ].
  aChar == $= ifTrue:[ | savePos |
    savePos := position .
    (self nextMatching:'end') ifTrue:[
       self position: savePos - 1 .  "backup so next will be $="
       ^ eolCount 
    ].
  ]
].
% 


!--------------------------------------------
set class FileSourceStream
category: 'Private'

method:
initializeWith: aGsFile

| status |
self initialize .

input := aGsFile .
inputSize := aGsFile fileSize .
bufSize := inputSize min: NormalBufSize .
buf := String new: bufSize .
status := self _readNextChunk .
%

method:
_seek: aPosition

"set position of a GsFile input, returns self "
| status |
status := input position: aPosition .
status == aPosition ifFalse:[  | fileErr |
  fileErr := input lastErrorString .
  self error:'File Seek Error', fileErr
].
%

method:
_readNextChunk

"read next chunk from a GsFile, returns number of bytes read"
| numToRead status |
numToRead := bufSize min: (inputSize - position + 1) .
status := input next: numToRead ofSize: 1 into: buf .
status = numToRead ifFalse:[  | fileErr |
  fileErr := input lastErrorString .
  self error:'File Read Error', fileErr
].
^ numToRead
%
