(as yet unclassified)
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

