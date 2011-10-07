
set class CZstream
category: '*maglev-runtime'
method:
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

%


set class CZstream
category: '*maglev-runtime'
method:
close
 | envId |
 envId := 1"__callerEnvId" .
 forWrite ifTrue:[ 
   self _flush: nil env: envId .
   self _streamOp: 1 with: nil . "close underlying stream"
 ] ifFalse:[ 
   self _finishReading
 ]

%


set class CZstream
category: '*maglev-runtime'
method:
flush
  "a ruby primitive"
  | bs  done envId |
  envId := 1"__callerEnvId" .
  (bs := bufSize) > 0 ifTrue:[
    comprBuffer ifNil:[ comprBuffer := String new: (bs max: 256) ].
    self _compressAndWriteFrom: buffer count: bs env: envId
  ].
  done := false .
  [ done ] whileFalse:[ | csiz |
    comprBuffer ifNil:[ comprBuffer := String new: 256 ].
    done := self _streamOp: 2 with: nil .  "deflate(zstream, Z_FINISH)"
    (csiz := comprSize) > 0 ifTrue:[ | cbuf savSiz status  |
      savSiz :=  (cbuf := comprBuffer) size .
      cbuf size: csiz .
      status := ioObj @ruby1:write: cbuf . "cannot  use write:from: with ruby "
      status ifNil:[ errorClass signal: 'error on output object' ].
      comprSize := 0 .
      done ifFalse:[  cbuf size: savSiz ].
    ].
  ].
  self _streamOp: 1 with: nil .  "zstream close"

%


set class CZstream
category: '*maglev-runtime'
method:
flush: flagsInt
  "a Ruby primitive.
   flagsInt may be nil (implies Z_FINISH)
   or should be Z_FLUSH_SYNC ."

^ self _flush: flagsInt env: 1"__callerEnvId"

%


set class CZstream
category: '*maglev-runtime'
method:
header
  "A ruby primitive.
   Returns an Array, { originalName . comment . mtime }
   from header of a gzip file, or signals an error.  mtime is a time_t as an Integer"
  forWrite ifTrue:[ errorClass signal:'stream not opened for reading'].
  header ifNil:[
    self _readAndDecompress: 1"__callerEnvId"  .
    header == false ifTrue:[ errorClass signal:'stream has no gzip header'].
  ].
  ^ header

%


set class CZstream
category: '*maglev-runtime'
method:
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

%


set class CZstream
category: '*maglev-runtime'
method:
readAll
  self error:'not supported for Ruby'

%


set class CZstream
category: '*maglev-runtime'
method:
rubyWrite: aString  count: numBytes
  "a ruby primitive"
  ^ self write: aString  count: numBytes env: 1"__callerEnvId"

%


set class CZstream
category: '*maglev-runtime'
method:
write: aString 
 "called from smalltalk.
  aString may also be aByteArray"
  ^ self write: aString count: aString size env: 1"__callerEnvId"

%


set class CZstream
category: '*maglev-runtime'
method:
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

%


set class CZstream
category: '*maglev-runtime'
method:
_compressAndWriteFrom: inputObj count: numBytes
  self error:'not supported for Ruby, use _compressAndWriteFrom:count:env:'

%


set class CZstream
category: '*maglev-runtime'
method:
_compressAndWriteFrom: inputObj count: numBytes env: envId
  | ofs | 
  ofs := 1 .
  [ true ] whileTrue:[ | csiz  |
    (csiz := comprSize) > 0 ifTrue:[ | cbuf |
      csiz == (cbuf := comprBuffer) size ifTrue:[ | status |
         "cannot use IO>>write:from: " 
         status := ioObj @ruby1:write: cbuf .
        status ifNil:[ errorClass signal: 'error on output object' ].
        comprSize := 0
      ].
    ].
    ofs > numBytes ifTrue:[
      ^ self "DONE"
    ].
    comprBuffer ifNil:[ comprBuffer := String new: ComprBufSize ].
       "compress primitive updates comprSize to reflect bytes added
         to comprBuffer ."
    ofs := self _compress: inputObj from: ofs to: numBytes .
  ]

%


set class CZstream
category: '*maglev-runtime'
method:
_flush: flagsInt env: envId
  "flagsInt may be nil (implies Z_FINISH)
   or should be Z_FLUSH_SYNC ."
  | bs  done |
  (bs := bufSize) > 0 ifTrue:[
    comprBuffer ifNil:[ comprBuffer := String new: (bs max: 256) ].
    self _compressAndWriteFrom: buffer count: bs env: envId .
    bufSize := 0 .
  ].
  done := false .
  [ done ] whileFalse:[ | csiz |
    comprBuffer ifNil:[ comprBuffer := String new: 256 ].
    done := self _streamOp: 2 with: flagsInt .  "deflate(zstream, flags)"
    (csiz := comprSize) > 0 ifTrue:[ | cbuf savSiz status  |
      savSiz :=  (cbuf := comprBuffer) size .
      cbuf size: csiz .
      status := ioObj @ruby1:write: cbuf . "cannot  use write:from: with ruby "
      status ifNil:[ errorClass signal: 'error on output object' ].
      comprSize := 0 .
      done ifFalse:[  cbuf size: savSiz ].
    ].
  ].

%


set class CZstream
category: '*maglev-runtime'
method:
_readAndDecompress
  self error:'not supported for Ruby, use _readAndDecompress:'

%


set class CZstream
category: '*maglev-runtime'
method:
_readAndDecompress: envId
  "Returns the number of available bytes in uncompressed  buffer,
   or zero if at EOF "
 | bs |
 bs := bufSize .
 [ bs == 0 ] whileTrue:[ 
    comprOffset > comprSize ifTrue:[ | count cbuf |
      cbuf := ioObj @ruby1:read: ComprBufSize .
      count := cbuf size .
      count == 0 ifTrue:[ ^ 0 "EOF" ].
      comprOffset := 1 . 
      comprBuffer := cbuf .
      comprSize := count .
    ].
    bs := self _decompress
  ].
  ^ bs 

%

