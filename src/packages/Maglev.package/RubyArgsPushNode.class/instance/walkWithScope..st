(as yet unclassified)
walkWithScope: aScope
   walked ifNil:[ 
     walked := true .
     ^ super walkWithScope: aScope
   ]

