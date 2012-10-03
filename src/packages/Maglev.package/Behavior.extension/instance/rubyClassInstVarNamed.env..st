*maglev-runtime
rubyClassInstVarNamed: aSymbol env: envId 
  | ns assoc   |
   ns := self nameSpace: envId .
   ns ifNotNil:[ assoc := ns resolveConstant: aSymbol ] .
   assoc ifNil:[ ^ nil ].
   ^ assoc _value

