*maglev-runtime
removeConst: aSymbol env: envId
  | rns |
  rns := self transientNameSpace: envId .
  rns ifNil:[
    rns := self persistentNameSpace: envId .
    (rns includesKey: aSymbol) ifFalse:[
      NameError signal: 'uninitialized constant object ', aSymbol.
    ].
  ].
  self _incrementCachedConstantsSerialNum .
  ^ rns removeConst: aSymbol .

