*maglev-runtime
lexLevelIsInline: aLevel
   "return true if specified level is in-line
       first level is 0 , stored at stk at:1"
  aLevel == 0 ifTrue:[ ^ true ].
  ^ (( lexLevelStack at: (aLevel + 1)   ) bitAnd: 16r10000) ~~ 0

