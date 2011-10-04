
doit
RubyListNode subclass: 'RubyDStrNode'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDStrNode
removeallmethods
removeallclassmethods

set class RubyDStrNode class
category: '*maglev-ast'
method:
s_a: anArray 
  | res |
  anArray _isArray ifFalse:[
     ArgumentError signal:'RubyDStrNode s_a, arg not an Array'
  ].
  (res := self _basicNew) list: anArray .
  ^ res

%


set class RubyDStrNode
category: '*maglev-ast'
method:
appendToHeadString: aString
  (list at: 1) appendString: aString

%


set class RubyDStrNode
category: '*maglev-ast'
method:
appendToList: aNode
  list add: aNode

%


set class RubyDStrNode
category: '*maglev-runtime'
method:
asDSymbolNode
  ^ RubyDSymbolNode _basicNew list: list shallowCopy 

%


set class RubyDStrNode
category: '*maglev-ast'
method:
dstrList
  ^ list

%


set class RubyDStrNode
category: '(as yet unclassified)'
method:
irNode
      "ruby_selector_suffix dependent"
    | node array lst |
    array := GsComArrayBuilderNode new.
    lst := list .
    1 to: lst size do:[ :n| | elemIr |
        elemIr := (lst at: n) irNode .
        elemIr ifNotNil:[ array appendElement: elemIr].
    ].
    (node := GsComSendNode new)
            rcvr: array;
            rubySelector:  #'__joinStrings#0__' .
    self ir: array; ir: node.
    ^ node

%


set class RubyDStrNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
	aStream nextPutAll: '('.
	list do: [:ea | aStream printNode: ea] separatedBy: [aStream nextPutAll: ' + '].
	aStream nextPutAll: ')'

%


set class RubyDStrNode
category: '*maglev-ast'
method:
size
  ^ list size

%


set class RubyDStrNode
category: '*maglev-ast'
method:
str_dstr_evstr_kind
  ^ 1

%


set class RubyDStrNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:dstr, ', self _inspect_list , $]

%

