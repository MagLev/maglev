as yet unclassified
walkWithScope: aScope isDefinedQ: isQ 

  ^ isQ ifTrue:[ self walkForDefinedQ: aScope ] 
        ifFalse:[ self walkWithScope: aScope]

