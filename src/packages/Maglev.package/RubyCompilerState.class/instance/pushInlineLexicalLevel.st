as yet unclassified
pushInlineLexicalLevel
  "increment lex level for an inline block"
  | lev mask |
  lexLevel := (lev := lexLevel + 1 ).
  mask := 16r10000 .  "the inline bit"
  lev >= mask ifTrue:[ self error:'nesting too deep' ].
  lexLevelStack push: ( lev bitOr: mask) .
  ^ lev 

