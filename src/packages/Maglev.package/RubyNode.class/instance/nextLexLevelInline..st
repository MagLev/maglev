as yet unclassified
nextLexLevelInline: aBlock
	| level cmState res  |
	cmState := RubyCompilerState current .
	level := cmState pushInlineLexicalLevel .
	[ 
	  res := aBlock value: level
	] ensure:[
	  cmState popLexicalLevel: level 
	].
     ^ res

