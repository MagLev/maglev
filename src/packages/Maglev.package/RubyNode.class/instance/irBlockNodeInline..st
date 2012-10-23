*maglev-runtime
irBlockNodeInline: parentNode
   "parentNode not used if receiver is a RubyNode"
    ^ self newInlineBlock:
       [:block | | list |
         self irNodeListInto: block .
         block
       ]

