*maglev-runtime
_rubyClassVarGet: aSymbol env: envId

|  assoc  |
(aSymbol size >= 2 and:[ aSymbol at:1 equals: '@@']) ifTrue:[
  assoc := self _rubyClassVarAssociation: aSymbol env: envId .
  assoc ifNotNil:[ ^ assoc _value ].
].
NameError signal: 'undefined class variable ', aSymbol .
^ nil

