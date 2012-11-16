*maglev-runtime
atEnd
"A ruby primitive.
 Returns true if an input stream is at eof, or has been closed by
  _finishReading"

 ioObj ifNotNil:[ | bs |
   bufSize > 0 ifTrue:[ ^ false ].
   bs := self _readAndDecompress: 1"__callerEnvId" .
   ^ bs == 0 
 ]. 
 ^ true

