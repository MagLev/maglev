*maglev-runtime
flush: flagsInt
  "a Ruby primitive.
   flagsInt may be nil (implies Z_FINISH)
   or should be Z_FLUSH_SYNC ."

^ self _flush: flagsInt env: 1"__callerEnvId"

