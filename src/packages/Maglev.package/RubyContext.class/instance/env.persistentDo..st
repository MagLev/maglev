*maglev-runtime
env: envId persistentDo: aBlock 
  "returns result of the one arg block aBlock"
| cst savePm |
cst := RubyCompilerState initialize: envId .
savePm := cst persistenceMode .
^ [ cst persistenceMode: true .
    aBlock value 
  ] ensure:[
    cst persistenceMode: savePm .
  ]

