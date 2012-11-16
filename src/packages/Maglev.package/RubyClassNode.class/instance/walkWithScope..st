(as yet unclassified)
walkWithScope: aScope
  | n |
  (n := superNode) ifNotNil: [ n walkWithScope: aScope ]. 
  super walkWithScope: aScope

