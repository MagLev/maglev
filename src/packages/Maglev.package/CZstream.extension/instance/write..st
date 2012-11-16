*maglev-runtime
write: aString 
 "called from smalltalk.
  aString may also be aByteArray"
  ^ self write: aString count: aString size env: 1"__callerEnvId"

