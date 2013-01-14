*maglev-runtime
walkWithScope: aScope
  resTmp := aScope newEvaluationTemp .
  ^ super walkWithScope: aScope

