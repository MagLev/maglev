
set class RubyListNode
category: 'accessing'
method:
do: aBlock	
	list do: aBlock

%


set class RubyListNode
category: 'parsetree'
method:
isSameAs: other
	^ true

%


set class RubyListNode
category: 'accessing'
method:
list

	 ^ list

%


set class RubyListNode
category: 'accessing'
method:
list: anArray
	list := anArray

%


set class RubyListNode
category: 'printing'
method:
printSourceOn: aStream
	list do: [:ea | aStream printNode: ea] separatedBy: [aStream nextPutAll: ', ']

%


set class RubyListNode
category: 'accessing'
method:
size
	^ list size

%


set class RubyListNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
   | lst |
   lst := list .
   1 to: lst size do:[:n |
     (lst at: n) walkWithScope: aScope
   ].

%


set class RubyListNode
category: '*maglev-ast'
method:
_append: aNode
  | lst |
  (lst := list) ifNil:[
    list := { aNode }
  ] ifNotNil:[
    lst add: aNode
  ] .
  ^ self

%


set class RubyListNode
category: '*maglev-runtime'
method:
_inspect_list
^ self _inspect_list: ', '

%


set class RubyListNode
category: '*maglev-runtime'
method:
_inspect_list: separator
  ^ self _inspect_list: separator parent: nil

%


set class RubyListNode
category: '*maglev-runtime'
method:
_inspect_list: separator parent: aParent
  | res sz |
  res := String new .
  1 to: (sz := list size) do:[:j | | elem str |
     elem := list at: j . 
     str := aParent ifNotNil:[ 
         elem == aParent ifTrue:[ '<recursiveList>' ] ifFalse:[ elem _inspect]
     ] ifNil:[ 
         elem _inspect 
     ] .
     res addAll: str .
     j < sz ifTrue:[ res addAll: separator ].
  ].
  ^ res

%

