as yet unclassified
lexicalSelfStack
	| stk |
	(stk := evalLexicalSelfStack ) ifNil:[
		self initialize: envId .
		stk := evalLexicalSelfStack .
	].
	^ stk 

