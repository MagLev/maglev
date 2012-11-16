(as yet unclassified)
walk: childrenList withScope: aScope
  1 to: childrenList size do:[:j| | ea |
    (ea := childrenList at: j) ~~ nil ifTrue:[ ea walkWithScope: aScope ]
  ].

