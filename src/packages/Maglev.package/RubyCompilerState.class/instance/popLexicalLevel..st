as yet unclassified
popLexicalLevel: lev 
  |  topLev  |
  topLev := lexLevelStack pop .
  (topLev bitAnd: 16rFFFF) == lev ifFalse:[ self error:'bad level in popLexicalLevel:'].
  lexLevel == lev  ifFalse:[ self error:'bad level in popLexicalLevel:'].
  lexLevel := lev - 1 .

