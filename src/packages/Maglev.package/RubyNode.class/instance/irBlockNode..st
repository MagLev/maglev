*maglev-runtime
irBlockNode: parentNode
    "parentNode not used here"
    ^ self newBlock:
       [:block | | list sz  |
          self irNodeListInto: block .
          block 
       ]

