
doit
RubyListNode subclass: 'RubyArrayNode'
	instVarNames: #( walked)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyArrayNode
removeallmethods
removeallclassmethods

set class RubyArrayNode class
category: '*maglev-ast'
method:
_new
| res |
res := self _basicNew .
res list: #() .  "note #() is canonicalized across image"
^ res

%


set class RubyArrayNode class
category: '*maglev-ast'
method:
_new: arg
| res |
res := self _basicNew .
res list: { arg } .
^ res

%


set class RubyArrayNode class
category: '*maglev-ast'
method:
_new: argOne with: argTwo
| res |
res := self _basicNew .
res list: { argOne . argTwo } .
^ res

%


set class RubyArrayNode
category: '(as yet unclassified)'
method:
argNodes
    walked := true .
    ^ list ifNil:[ #() ]

%


set class RubyArrayNode
category: '*maglev-ast'
method:
arrayLength
  ^ list size

%


set class RubyArrayNode
category: 'as yet unclassified'
method:
asRpMasgnRhs 
   ^ false "no changes"

%


set class RubyArrayNode
category: '(as yet unclassified)'
method:
buildIrLeafsInto: anArray
  | lst |
  lst := list .
  1 to: lst size do:[:n |
    (lst at: n) buildIrLeafsInto: anArray
  ].

%


set class RubyArrayNode
category: 'as yet unclassified'
method:
getClearIter
  ^ nil

%


set class RubyArrayNode
category: 'as yet unclassified'
method:
hasRestArg
	^ false

%


set class RubyArrayNode
category: '(as yet unclassified)'
method:
irNode
  | ary lst |
  ary := GsComArrayBuilderNode new.
  lst := list .
  1 to: lst size do:[:n |
    ary appendElement: ( lst at: n) irNode
  ].
  self ir: ary.
  ^ ary

%


set class RubyArrayNode
category: 'as yet unclassified'
method:
printArgsOn: aStream
	list ifNotNil:
		[list do: [:ea | aStream printNode: ea] separatedBy: [aStream nextPutAll: ',']]

%


set class RubyArrayNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
	aStream nextPut: $[.
	self printArgsOn: aStream.
	aStream nextPut: $]

%


set class RubyArrayNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
   walked ifNil:[ 
     walked := true .
     ^ super walkWithScope: aScope
   ]

%


set class RubyArrayNode
category: 'as yet unclassified'
method:
yieldArgsList
  ^ { self }

%


set class RubyArrayNode
category: '*maglev-ast'
method:
_append: aNode
  | lst |
  (lst := list) == #() ifTrue:[
    list := { aNode }
  ] ifFalse:[
    lst add: aNode
  ] .
  ^ self

%


set class RubyArrayNode
category: '*maglev-ast'
method:
_appendAll: anArrayNode
  | lst args |
  args := anArrayNode list .
  args size ~~ 0 ifTrue:[
    (lst := list) == #() ifTrue:[
      list := args copy
    ] ifFalse:[ 
      lst addAll: args
    ]
  ].
  ^ self

%


set class RubyArrayNode
category: '*maglev-runtime'
method:
_appendAmperLhs: aNode
  aNode setHasAmpersand . 
  ^ self _append: aNode

%


set class RubyArrayNode
category: '*maglev-runtime'
method:
_inspect 
  ^ '[:array ,' , self _inspect_list , $]

%


set class RubyArrayNode
category: '*maglev-ast'
method:
_prepend: aNode
  | lst |
  (lst := list) == #() ifTrue:[
     list := { aNode }
  ] ifFalse:[
    lst insertAll: { aNode } at: 1
  ] .
  ^ self 

%

