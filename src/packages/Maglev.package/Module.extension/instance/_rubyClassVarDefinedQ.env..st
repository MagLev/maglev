*maglev-runtime
_rubyClassVarDefinedQ: aSymbol env: envId
  "Return nil  or  'class-variable' "
|  assoc  |
(aSymbol size > 2 and:[ aSymbol at:1 equals: '@@']) ifTrue:[
  assoc := self _rubyClassVarAssociation: aSymbol env: envId .
  assoc ifNotNil:[ ^ assoc definedQ ].
].
^ nil

