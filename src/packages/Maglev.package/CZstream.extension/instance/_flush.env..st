*maglev-runtime
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

