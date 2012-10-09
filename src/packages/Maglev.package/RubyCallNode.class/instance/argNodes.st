(as yet unclassified)
argNodes
    | itr args |
    args := argsNode fCallArgNodes .
    (itr := iterNode) ifNotNil:[
      (args := args copy) add: itr . 
    ].
    ^ args

