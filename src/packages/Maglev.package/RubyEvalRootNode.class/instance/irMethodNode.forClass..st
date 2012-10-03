*maglev-runtime
irMethodNode: envId forClass: aClass 
 
  ^ self buildIrMethodNode: [ :node |
      staticScope buildTempsOn: node.
      self useScope: staticScope during: [ | nlist |
        nlist := bodyNode irNodeListInto: { } .
        node appendStatement: (nlist at: 1) .
        node appendStatement:
            (self newBlock: [:block | | rtn lstSiz lst |
                lst := nlist .
                lstSiz := lst size . 
                2 to: lstSiz - 1 do:[:j | block appendStatement:( lst at: j) ]. 
                rtn := self ir: (lst at: lstSiz) returnNode .
                block appendStatement: rtn .
                block returnNode
             ])
        ].
     ]

