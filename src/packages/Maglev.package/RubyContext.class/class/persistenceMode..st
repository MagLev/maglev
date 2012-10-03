*maglev-runtime
persistenceMode: aBoolean
  "a Ruby primitive. 
   true means  inside of a Maglev.persistent  block.  
   Returns the previous state of the persistenceMode .  "
  | prev envId |
  prev := RubyCompilerState current persistenceMode: aBoolean .
  (prev == true  and:[ aBoolean ~~ true ]) ifTrue:[ | path |
     envId := 1"__callerEnvId" .
     path := RubyContext transientLoadPathCopy: envId .
    (Object persistentNameSpace: envId) rubyGlobalVar: #'$:' put: path . 
  ].

