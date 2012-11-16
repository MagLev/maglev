(as yet unclassified)
hasRestArg
  | sel sz |

  sel := methSelector .
  sz := sel size. 
  (sel at: sz) == $* ifTrue:[ ^ true ].
  (sz >= 2 and:[ (sel at: sz - 1) == $*]) ifTrue:[ ^ true ].
  ^ false 

