*maglev-runtime
_rubyConstantsFreeze
  "a ruby primitive.
   intended for use during bootstrap loading only"
  | ns | 
  RubyCompilerState current reloadingPrims ifFalse:[
    (ns := self persistentNameSpace: 1"__callerEnvId") ifNotNil:[ 
      ns _rubyConstantsFreeze 
    ].
  ]

