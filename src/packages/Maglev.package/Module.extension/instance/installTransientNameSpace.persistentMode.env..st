*maglev-runtime
installTransientNameSpace: tns persistentMode: pm env: envId 
  "for use at module/class creation. incoming tns maybe created by parser"
  | pns |
  tns initializeForModule: self env: envId .
  self transientNameSpace: envId put: tns .
  pm ifTrue:[
    (pns := tns persistentCopy) ifNil:[ 
	   pns := RubyNameSpace new .
	   tns persistentCopy: pns .
	 ].
    pns initializeForModule: self env: envId .
    self persistentNameSpace: envId put: pns 
  ].

