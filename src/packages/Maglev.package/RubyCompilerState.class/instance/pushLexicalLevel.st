as yet unclassified
pushLexicalLevel
  "increment lex level for a non-inline block"
  | lev |
  lexLevel := (lev := lexLevel + 1 ).
  lexLevelStack push: lev .
  ^ lev 

