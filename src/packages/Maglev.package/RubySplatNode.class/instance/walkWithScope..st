(as yet unclassified)
walkWithScope: aScope
  evalTmp := aScope newEvaluationTemp .
  walked ifNil:[ 
    walked := true .
    node walkWithScope: aScope  .
  ].

