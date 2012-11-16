(as yet unclassified)
walkWithScope: aScope
  | args sz nTmps evalTmps |
  args := argsNode argNodes .  
  argsList := args . 
  sz  := args size .
  nTmps := 2 "for rcvr, result" . 
  1 to: sz do:[:m | 
     (args at: m) walkWithScope: aScope .
     nTmps := nTmps + 1 .
  ].
  valueNode walkWithScope: aScope .
  receiverNode walkWithScope: aScope .
  evalTmps := Array new: nTmps .
  1 to: nTmps do:[:k |  evalTmps at: k put: aScope newEvaluationTemp ].
  evalTmpAssocs := evalTmps .

