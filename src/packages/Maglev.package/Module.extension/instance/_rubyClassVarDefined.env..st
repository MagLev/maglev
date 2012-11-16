*maglev-runtime
_rubyClassVarDefined: aSymbol  env: envId
  "Return true or false if receiver defines the specified variable.
   does not search hierarchy."
|  assoc  ns |
(aSymbol size > 2 and:[ aSymbol at:1 equals: '@@']) ifTrue:[ 
  ( ns := self _classForRubyClassVar nameSpace: envId ) ifNotNil:[
    assoc := ns resolveConstant: aSymbol .
    assoc ifNotNil:[ ^ true ] .
  ].
  ^ false .
].
NameError signal:'invalid class variable name' .
^ false

