*maglev-runtime
walkWithScope: aScope
  | lpv |
  lpv := self walkEvalCall: aScope .
  lexPathVar := lpv .
  lpv walkWithScope: aScope .
  ^ super walkWithScope: aScope 

