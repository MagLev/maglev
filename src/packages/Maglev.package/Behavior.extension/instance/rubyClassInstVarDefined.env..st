*maglev-runtime
rubyClassInstVarDefined: aSymbol env: envId 
	| ns assoc   |
   ns := self nameSpace: envId .
   ns ifNotNil:[ assoc := ns resolveConstant: aSymbol ] .
   assoc ifNil:[ ^ nil ].
   assoc _valueNoAction  ifNotNil:[ ^ #'instance-variable' ] .
   ^ nil 
  

