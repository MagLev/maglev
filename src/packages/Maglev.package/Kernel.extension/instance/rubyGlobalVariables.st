*maglev-runtime
rubyGlobalVariables
  "a ruby primitive"
  | ns arr |
  ns := Object transientNameSpace: 1"__callerEnvId"  .
  arr := { } .
  ns keysAndValuesDo:[ :aKey :aVal |  
    (aKey at: 1) ==  $$  ifTrue:[ arr add: (String withAll: aKey)].
  ].
  "  #'$?'  not currently included  , it is a runtime dynamic value"
  ^ arr 

