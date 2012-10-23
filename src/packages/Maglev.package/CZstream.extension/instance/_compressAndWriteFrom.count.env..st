*maglev-runtime
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

