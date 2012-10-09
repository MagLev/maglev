(as yet unclassified)
walkWithScope: aScope
  | lst sz |
  evalTemps := Array new:( sz := self evalTempsSize ) .
  1 to: sz do:[:n | evalTemps at: n put: aScope newEvaluationTemp ]. 

  lst := leftList .
  1 to: lst size do:[:n| (lst at:n) walkWithScope: aScope ].
  lst := rightList .
  1 to: lst size do:[:n| (lst at:n) walkWithScope: aScope ].

