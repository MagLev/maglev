*maglev-cextensions
cextGlobalVariables: envId 
  | ns arr |
  ns := Object transientNameSpace: envId .
  arr := { } .
  ns keysAndValuesDo:[ :aKey :aVal |  
    (aKey at: 1) ==  $$  ifTrue:[ arr add: (String withAll: aKey)].
  ].
  "  #'$?'  not currently included  , it is a runtime dynamic value"
  ^ arr 

