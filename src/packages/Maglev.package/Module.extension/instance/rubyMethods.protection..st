*maglev-runtime
rubyMethods: includeSuper protection: protInt
  "a ruby primitive"
  | cls |
   (cls := self moduleMethodsModuleOrNil) ifNotNil:[ 
     ^ cls rubyMethods: includeSuper protection: protInt env: 1"__callerEnvId"
   ] ifNil:[ 
     ^ IdentitySet new 
   ]

