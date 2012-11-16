(as yet unclassified)
walkWithScope: aScope
  | mth scp |
  mth := RubyCompilerState current topMethodDef .
  scp := mth scope .
  firstTmp := scp newEvaluationTemp .
  firstNode walkWithScope: aScope .
  secondNode walkWithScope: aScope .

