*maglev-runtime
rubyMethods: includeSuper protection: protInt env: envId
  "Return an IdentitySet of Symbols "
| set curClass  hidden inclSuper |
  set := IdentitySet new . hidden := IdentitySet new .
  self nonBridgeRubySelectorsInto: set hiddenInto: hidden 
	  protection: protInt env: envId .
  includeSuper  ifTrue:[
    curClass := self .
    [ true ] whileTrue:[  
       (curClass := curClass rubySuperclass: envId) ifNil:[ ^ set ]. 
       curClass == Object ifTrue:[ ^ set  "Fix Trac 719" ].
       (curClass isRubyModuleInclude or: [curClass == Module]) ifTrue:[
          curClass nonBridgeRubySelectorsInto: set hiddenInto: hidden 
			protection: protInt env: envId 
       ].
     ].
  ].
  ^  set 

