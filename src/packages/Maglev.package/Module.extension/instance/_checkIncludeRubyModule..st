*maglev-runtime
_checkIncludeRubyModule: aModule 
  "a ruby primitive"
  | envId |
  envId := 1"__callerEnvId" .
  (aModule class == Module or:[aModule == Kernel] )  ifFalse:[
    ((aModule isKindOf: Module) and:[ (aModule isKindOf: Metaclass3) not]) ifFalse:[
      ArgumentTypeError signal:'argument to include is not a Module'
    ].
  ].
 aModule == self ifTrue:[ 
   self moduleIncludeSelf: envId   .
   ^ false "caller should not call __include"
 ].
 ^ true

