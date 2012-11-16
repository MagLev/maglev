*maglev-runtime
loadFileNamed: aString
  "a ruby primitive"
     ^ self loadFileNamed: aString env: 1"__callerEnvId" 

