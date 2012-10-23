*maglev-runtime
moduleFrozen
  "a ruby primitive"
  | ns |
  ns := self nameSpace: 1"__callerEnvId" .
  ns ifNotNil:[ ^ ns moduleFrozen ].
  ^ false

