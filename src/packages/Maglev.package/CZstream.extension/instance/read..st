*maglev-runtime
read: maxBytes
  "A ruby primitive.
   read and decompress up to maxBytes from receiver,  
   returning a String which may be smaller than maxBytes.
   Result will be nil if EOF hit."
  | res bs |
  maxBytes <= 0 ifTrue:[
     maxBytes < 0 ifTrue:[ ArgumentError signal:'maxBytes must be >= 0'].
     ^ String new . 
  ].
  (bs := bufSize) == 0 ifTrue:[
    bs := self _readAndDecompress: 1"__callerEnvId" .
    bs == 0 ifTrue:[ ^ nil  ].
  ].
  maxBytes >= bs ifTrue:[
    res := buffer .
    res size: bs .
    buffer := nil .
    bufSize := 0 .
  ] ifFalse:[
    res := buffer copyFrom:1 to: maxBytes .
    buffer replaceFrom: 1 to: bs - maxBytes with: buffer startingAt: maxBytes + 1 .
    bufSize := bs - maxBytes . 
  ].
  ^ res

