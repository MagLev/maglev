as yet unclassified
irGotoNodeFrom: aLoop to: aLabel
  | node |
  (node := GsComGotoNode new)  "goto within the same in-line loop"
     localRubyBreak: aLabel  .
   ^ node

