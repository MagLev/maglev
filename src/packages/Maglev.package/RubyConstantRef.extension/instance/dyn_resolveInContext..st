*maglev-runtime
dyn_resolveInContext: anObject
  "called from generated code"
  | idx mySize assoc val envId aModule sym |
"evaluate  :: terms"  
aModule := anObject .
assoc := anObject  rubyConstAssociationAt: (sym := self at: 1)  env: (envId := 1"__callerEnvId") . 
mySize := self size .   
idx := 2 .
[ idx <= mySize ] whileTrue:[ | prev |
	aModule := assoc _valueFor: sym inClass: aModule env: envId . "possible autoload"
  assoc := aModule rubyConstAssociationAt: (sym := self at: idx) env: envId .
  idx := idx + 1
].
"no setGlobalAssoc...  , result is not cachable due to first term not a constant name"
val := assoc _valueFor: sym inClass: aModule env: envId . "possible trigger of autoload"

^ val

