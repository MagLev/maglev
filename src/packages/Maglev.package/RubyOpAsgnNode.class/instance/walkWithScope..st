(as yet unclassified)
walkWithScope: aScope
  rcvrTmp := aScope newEvaluationTemp .
  valueNode walkWithScope: aScope .   "argNodes not used/implemented"
  receiverNode walkWithScope: aScope .

