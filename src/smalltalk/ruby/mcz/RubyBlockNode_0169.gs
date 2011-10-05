
set class RubyBlockNode class
category: '*maglev-ast'
method:
s_a: aNode
  ^ self _basicNew list: { aNode } .

%


set class RubyBlockNode class
category: '*maglev-ast'
method:
s_list: anArray
  ^ self _basicNew list: anArray

%


set class RubyBlockNode
category: '*maglev-ast'
method:
append_to_block: aNode
  aNode == self ifTrue:[ self error:'recursive block A'].
  list add: aNode .
  ^ self

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
irBeginBodyNode
  "receiver is body of a begin..end that contains no rescue or ensure"
  | node |
  (node := GsComSendNode new)
     rcvr:  (self irBlockNode: self)  ;
     stSelector: #value  .
  self ir: node .
  ^ node

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
irBlockPassNode
  ^ self irNode

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
irEvaluateBlock
  "generate a send to evaluate a block "
      "ruby_selector_suffix dependent"
  | irBlk send |
  irBlk := self irNode .
  ( send := GsComSendNode new)
     rcvr: irBlk ;
     rubySelector:  #'call#0__'   .
  self ir: send . 
  ^ send 

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
irEvaluatedBlockNode
 " typical use:  IR generation for a Ruby begin..end  that does not contain a rescue keyword "
  | node lst |
  lst := { } .
  self irNodeListInto: lst .
  (node := GsComStatementsNode new) list: lst .
  self ir: node .
  ^ node 

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
irEvaluatedRcvrNode
  ^ self irEvaluateBlock

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
irNode
  ^ self irBlockNode: self 

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
irNodeListInto: blockIr
  | lst |
  lst := list .
  1 to: lst size do:[:n | (lst at: n) irNodeListInto: blockIr ].

%


set class RubyBlockNode
category: 'parsetree-test'
method:
isSameAs: other
	^ true

%


set class RubyBlockNode
category: '*maglev-ast'
method:
prepend_to_block: aNode
  aNode == self ifTrue:[ self error:'recursive block B'].
  list insertAll: { aNode } at: 1  . 
  ^ self

%


set class RubyBlockNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
	list do: [:ea | aStream printNode: ea]

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
walkOptionalArgs: aScope
   | lst sz |
   lst := list .
   1 to: (sz := lst size) do:[:n |
     (lst at: n) walkOptionalArg: aScope
   ].
   1 to: sz do:[:n |
     (lst at: n) walkOptionalArgRhs: aScope
   ].

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
walkOptionalArgsLhs: aScope
   | lst |
   lst := list .
   1 to: lst size do:[:n |
     (lst at: n) walkOptionalArg: aScope
   ].

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
walkOptionalArgsRhs: aScope
   | lst |
   lst := list .
   1 to: lst size do:[:n |
     (lst at: n) walkOptionalArgRhs: aScope
   ].

%


set class RubyBlockNode
category: '*maglev-runtime'
method:
_inspect
  | sep |
  sep := ',
  '.
  ^ '
[:block, ' , (self _inspect_list: sep parent: self) , ']
'

%

