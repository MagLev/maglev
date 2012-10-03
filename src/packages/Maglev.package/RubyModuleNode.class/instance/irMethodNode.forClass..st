*maglev-runtime
irMethodNode: envId forClass: aClass 
  | irMeth |
  irMeth := self buildIrMethodNode: [ :node |
       scope buildTempsOn: node. 
       self useScope: scope during: [bodyNode buildStatementsOn: node].
  ].
  irMeth sourceOffset: position .
  ^ irMeth

