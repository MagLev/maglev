
set class UndefinedObject
category: '*maglev-ast'
method:
argNodes
	^ #()

%


set class UndefinedObject
category: '*maglev-runtime'
method:
buildStatementsOn: irNode    
    irNode appendStatement:  GsComLiteralNode newNil  returnNode

%


set class UndefinedObject
category: '*maglev-ast'
method:
detectMismatchWith: aNode
	^ (aNode isKindOf: RubyNilNode)
		ifFalse: [{self. aNode}]

%


set class UndefinedObject
category: '*maglev-ast'
method:
fCallArgNodes
  ^ #()

%


set class UndefinedObject
category: '*maglev-ast'
method:
irBlockNode: parentNode 
  ^ self irBlockNodeInline: parentNode

%


set class UndefinedObject
category: '*maglev-ast'
method:
irBlockNodeInline: parentNode
    ^ parentNode newInlineBlock:
	    [ :block |
		  block appendStatement: GsComLiteralNode newNil .
		  block
		].

%


set class UndefinedObject
category: '*maglev-runtime'
method:
irCaseNodeListWithLeaf: aLeaf into: blockIr
   blockIr appendStatement: GsComLiteralNode newNil

%


set class UndefinedObject
category: '*maglev-ast'
method:
irCaseNodeWithNode: aNode
	^ GsComLiteralNode newNil

%


set class UndefinedObject
category: '*maglev-runtime'
method:
irNodeListInto: blockIr
  ^ self  "nothing to add to block"

%


set class UndefinedObject
category: '*maglev-runtime'
method:
isSplatNode
  ^ false

%


set class UndefinedObject
category: '*maglev-ast'
method:
not
	^ true

%


set class UndefinedObject
category: '*maglev-runtime'
method:
walkWithScope: aScope
  ^ self

%


set class UndefinedObject
category: '*maglev-runtime'
method:
_inspect
  ^ self asString

%

