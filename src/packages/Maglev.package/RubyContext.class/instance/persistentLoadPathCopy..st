*maglev-runtime
persistentLoadPathCopy: envId
  | path mHome|
  "Copy the persistent version of the ruby load path to the transient state.
   Expand $MAGLEV_HOME for each element of the path.  Ensure both the array
   and the contents of the array are copied so nothing shared between transient
   and persistent copies."
  mHome := RubyEnv _getenv:'MAGLEV_HOME' .
  path := (Object persistentNameSpace: envId) rubyGlobalVar: #'$:' .
  ^ RubyContext copyPath: path replacing: '$MAGLEV_HOME' with: mHome .

