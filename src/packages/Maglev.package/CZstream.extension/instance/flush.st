*maglev-runtime
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

