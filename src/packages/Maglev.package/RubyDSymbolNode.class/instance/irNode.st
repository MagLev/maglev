(as yet unclassified)
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

