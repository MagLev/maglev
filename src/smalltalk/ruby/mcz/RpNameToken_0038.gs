
doit
Object subclass: 'RpNameToken'
	instVarNames: #( val src_offset quid)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RpNameToken
removeallmethods
removeallclassmethods

set class RpNameToken class
category: '*maglev-ast'
method:
new: sym position: ofs
  ^ self _basicNew sym: sym position: ofs 

%


set class RpNameToken
category: 'as yet unclassified'
method:
asClassNameNode
   | node |
  (node := RubyClassNameNode _basicNew)
     position: src_offset  ;
     name: val  . 
  ^ node

%


set class RpNameToken
category: '*maglev-ast'
method:
src_offset
  ^ src_offset

%


set class RpNameToken
category: '*maglev-ast'
method:
sym: aSymbol position: ofs
  val := aSymbol  .
  src_offset := ofs  .
  ^ self

%


set class RpNameToken
category: '*maglev-ast'
method:
symval
  ^ val

%


set class RpNameToken
category: '*maglev-runtime'
method:
_inspect
  ^ '(RpNToken "', val , '" quid 0x', quid asHexString , $)

%

