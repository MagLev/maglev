*maglev-runtime
rubyClassInstVarDefined: aSymbol env: envId 
	| ns assoc   |
   ns := self nameSpace: envId .
   ns ifNotNil:[ assoc := ns resolveConstant: aSymbol ] .
   assoc ifNil:[ ^ nil ].
   "Fix Github 209: ClassInstVars are defined, even if their value is nil."
   assoc ifNotNil:[ ^ #'instance-variable' ].
   ^ nil 
  

