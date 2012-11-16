*maglev-runtime
argIsSplatAt: idx
  argsNode isSplatNode ifTrue:[ ^ false ].
  ^ super argIsSplatAt: idx

