*maglev-runtime
lexLevelOkForFixedIvAccess
  self compilingEval ifTrue:[ ^ false ].
  (self lexLevelIsInlineWithin: 0 ) ifTrue:[ ^ true ]  .
  ^ installingPrims

