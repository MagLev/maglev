*maglev-runtime
write: aString  count: numBytes env: envId
 "aString may also be aByteArray"

 | bs |
 numBytes < HalfComprBufSize ifTrue:[
   (numBytes + (bs:= bufSize))  >  HalfComprBufSize ifTrue:[
     self _compressAndWriteFrom: buffer count: bs env: envId .
     bs := 0 .
     bufSize := 0 .
   ].
   buffer replaceFrom: bs + 1 to: bs + numBytes with: aString startingAt: 1 .
   bufSize := bs + numBytes .
 ] ifFalse:[
   (bs := bufSize) > 0 ifTrue:[
     self _compressAndWriteFrom: buffer count: bs env: envId .
     bufSize := 0 .
   ].
   self _compressAndWriteFrom: aString count: numBytes env: envId
 ]

