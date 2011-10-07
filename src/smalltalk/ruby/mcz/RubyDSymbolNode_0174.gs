
set class RubyDSymbolNode
category: '(as yet unclassified)'
method:
irNode
      "ruby_selector_suffix dependent"
    | node array lst |
    array := GsComArrayBuilderNode new.
    lst := list  .
    1 to: lst size do: [:n | array appendElement: (lst at: n) irNode].
    node := GsComSendNode new
            rcvr: array;
            rubySelector: #'__joinStringsAsSymbol#0__' .
    self ir: array; ir: node.
    ^ node

%


set class RubyDSymbolNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
   aStream nextPutAll: '('. 
   list do: [:ea | aStream printNode: ea] separatedBy: [aStream nextPutAll: ' + '].
   aStream nextPutAll: ')'

%


set class RubyDSymbolNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:dsym, ', self _inspect_list , $]

%

