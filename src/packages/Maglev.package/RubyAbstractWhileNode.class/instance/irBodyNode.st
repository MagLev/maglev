*maglev-runtime
irBodyNode
  ^ self newBlock: [:block |         
        block appendStatement:  labelRedo .                        
        bodyNode irNodeListInto: block .
        block appendStatement: labelNext .
       block 
     ] isInline: blksInline .

