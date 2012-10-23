*maglev-runtime
rubyWrite: aString  count: numBytes
  "a ruby primitive"
  ^ self write: aString  count: numBytes env: 1"__callerEnvId"

