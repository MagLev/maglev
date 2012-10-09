as yet unclassified
evalTempsSize
   "executed during walkWithScopes phase"
   | rsz r |
   ( r := rightList ) ifNotNil:[
     starSize :=  (rsz := r size) - (leftList size - 1) max: 0 .
     ^ rsz - starSize + 1 
   ] ifNil:[
      "starSize left as nil" 
     ^ 1   "for irAssignmentNode:"
   ]

