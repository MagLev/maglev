*maglev-runtime
_rubyClassVarGetOrNil: aSymbol env: envId
|  assoc  |
(aSymbol size > 2 and:[ aSymbol at:1 equals: '@@']) ifTrue:[
  assoc := self _rubyClassVarAssociation: aSymbol env: envId.
  assoc ifNotNil:[ ^ assoc _value ].
].
^ nil

