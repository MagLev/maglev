*maglev-runtime
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

