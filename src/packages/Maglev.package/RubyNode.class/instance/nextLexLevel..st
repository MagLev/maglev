as yet unclassified
nextLexLevel: aBlock
	| level cmState res  |
	cmState := RubyCompilerState current .
	level := cmState pushLexicalLevel .
	[ 
	  res := aBlock value: level
	] ensure:[
	  cmState popLexicalLevel: level 
	].
     ^ res

