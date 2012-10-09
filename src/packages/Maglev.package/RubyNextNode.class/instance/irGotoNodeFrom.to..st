as yet unclassified
irGotoNodeFrom: aLoop to: aLabel
  | node |
  (node := GsComGotoNode new)  "goto within the same in-line loop"
     localRubyNext: aLabel argForValue: aLabel argForValue .
   ^ node

