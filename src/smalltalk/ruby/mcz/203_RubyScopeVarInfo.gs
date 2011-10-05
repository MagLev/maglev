
doit
Object subclass: 'RubyScopeVarInfo'
	instVarNames: #( key kind leaf
	                  offsetInScop)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'Maglev-AST'
	options: #()

%

set class RubyScopeVarInfo
removeallmethods
removeallclassmethods

set class RubyScopeVarInfo class
category: '*maglev-runtime'
method:
comment
^ ' kinds are 
    #incomingBlock -  incoming block argument (&block) to a method definition
			(instance of RubyScopeIncomingBlockVar)
    #blockArg -  declared argument to a Block , like { |aBlockArg |  ... }
    #normal   -  method or block temp or , other incoming arg to method definition
    #evaluationTemp - temp created by IR generation, not declared in Ruby source
  '

%


set class RubyScopeVarInfo class
category: '*maglev-runtime'
method:
new: nameSym kind:  kindSym ofs: anOffset
  ^ self _basicNew key: nameSym kind: kindSym  ofs: anOffset

%


set class RubyScopeVarInfo
category: '*maglev-runtime'
method:
key
 ^ key

%


set class RubyScopeVarInfo
category: '*maglev-ast'
method:
key: aSymbol
  key := aSymbol

%


set class RubyScopeVarInfo
category: '*maglev-runtime'
method:
key: nameSym kind: kindSym ofs: anOffset
  key := nameSym .
  kind := kindSym .
  offsetInScop := anOffset .
  ^ self

%


set class RubyScopeVarInfo
category: '*maglev-runtime'
method:
kind
  ^ kind 

%


set class RubyScopeVarInfo
category: '*maglev-runtime'
method:
leaf
  ^ leaf 

%


set class RubyScopeVarInfo
category: '*maglev-runtime'
method:
leaf: aLeaf 
   leaf := aLeaf 

%


set class RubyScopeVarInfo
category: '*maglev-runtime'
method:
leafInScope: scope
  ^ leaf ifNil:[ | lf |
     lf := scope _newLeaf: key offsetInScope: offsetInScop kind: kind .
     leaf := lf .
     lf
  ]

%


set class RubyScopeVarInfo
category: '*maglev-runtime'
method:
readCount
  ^ 0

%


set class RubyScopeVarInfo
category: '*maglev-runtime'
method:
readCount: delta
  ^ 0  "not an incoming block arg"

%

