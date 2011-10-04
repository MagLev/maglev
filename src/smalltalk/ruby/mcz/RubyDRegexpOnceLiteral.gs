
doit
Object subclass: 'RubyDRegexpOnceLiteral'
	instVarNames: #( regex)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDRegexpOnceLiteral
removeallmethods
removeallclassmethods

set class RubyDRegexpOnceLiteral
category: '*maglev-runtime'
method:
regex
  ^ regex

%

