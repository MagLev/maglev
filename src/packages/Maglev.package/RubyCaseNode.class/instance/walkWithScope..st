(as yet unclassified)
walkWithScope: aScope
   
    caseBody walkWithScope: aScope .
    caseNode ifNotNil:[
	  evalTmp := aScope newEvaluationTemp .
	  caseNode walkWithScope: aScope
	 ].

