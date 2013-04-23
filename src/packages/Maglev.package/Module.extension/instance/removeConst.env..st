*maglev-runtime
removeConst: aSymbol env: envId
  | rns |
  rns := self transientNameSpace: envId .
  rns ifNil:[
    rns := self persistentNameSpace: envId .
    (rns includesKey: aSymbol) ifFalse:[
      NameError signal: 'uninitialized constant object ', aSymbol.
    ].
    RubyCompilerState current persistenceMode ifFalse:[
      "ArgumentTypeError signal:'cannot remove constant from persistent 
module in transient mode'"
    ].
  ].
  self _incrementCachedConstantsSerialNum .
  ^ rns removeConst: aSymbol .

