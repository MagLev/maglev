*maglev-runtime
irMethodNode: envId forClass: aClass 

  | irMeth |
  irMeth := self buildIrMethodNode: [ :node |  
      node forceAllArgsTmpsToVc . "patch for Trac678"
      self useScope: scope during: [  | blkarg | "always implicit block, Trac808"
        scope buildTempsOn: node.
        node appendArg: (blkarg := scope blockArgLeaf: nil ). 
        bodyNode buildStatementsOn: node .
      ].
   ].
   irMeth sourceOffset: position .
   ^ irMeth

