*maglev-ast
irNode
      "ruby_selector_suffix dependent"
    | node array lst |
    array := GsComArrayBuilderNode new.
    lst := list .
    1 to: lst size do: [:n | array appendElement: (lst at: n) irNode].
    (node := GsComSendNode new)
            rcvr: array;
            rubySelector: #'__joinStringsWithRegexpOptions#1__'  ;
            appendArgument: (RubyFixnumNode newForInt: self fixedOptions) irNode.
    self ir: array; ir: node.
    ^ node

