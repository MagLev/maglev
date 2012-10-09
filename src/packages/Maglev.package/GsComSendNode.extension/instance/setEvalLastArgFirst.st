*maglev-runtime
setEvalLastArgFirst
  self error:'UNEXPECTED use of Deprecated setEvalLastArgFirst'.
  envFlags := envFlags bitOr: EvalLastArgFirst_MASK

