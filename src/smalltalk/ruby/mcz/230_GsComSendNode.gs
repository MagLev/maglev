
set class GsComSendNode
category: '*maglev-runtime'
method:
setEvalLastArgFirst
  self error:'UNEXPECTED use of Deprecated setEvalLastArgFirst'.
  envFlags := envFlags bitOr: EvalLastArgFirst_MASK

%

