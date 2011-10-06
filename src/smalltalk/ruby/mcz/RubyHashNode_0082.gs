
set class RubyHashNode
category: '*maglev-ast'
classmethod:
s_a: list
  | res aryNodeCls lst_cls |
  res := self _basicNew .
  lst_cls := list class .
  lst_cls == (aryNodeCls := RubyArrayNode) ifTrue:[
    res listNode: list
  ] ifFalse:[
     lst_cls == RubyRpCallArgs ifTrue:[ | ary |
       (ary := aryNodeCls _basicNew) list: list list .
       res listNode: ary 
     ] ifFalse:[
       RubyParserM signalError: 'RubyHashNode.s bad arg'  .     
       ^ nil
     ]
  ].
  ^ res

%


set class RubyHashNode
category: '*maglev-runtime'
method:
isSmalltalkSend
    ^ false

%


set class RubyHashNode
category: 'accessing'
method:
listNode

	 ^ listNode

%


set class RubyHashNode
category: 'accessing'
method:
listNode: aListNode
	listNode := aListNode

%


set class RubyHashNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: '{';
		printNode: listNode;
		nextPutAll: '}'

%


set class RubyHashNode
category: 'converting'
method:
receiverNode
	^ listNode

%


set class RubyHashNode
category: '*maglev-runtime'
method:
selector
   ^ #__as_hash

%


set class RubyHashNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:hash, ', listNode _inspect  , $]

%

