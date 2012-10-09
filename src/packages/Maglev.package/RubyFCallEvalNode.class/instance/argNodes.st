*maglev-runtime
argNodes
    | itr args |
    args := argsNode fCallArgNodes .
    self evalAddFileLineArgs: args for: callName .
    (itr := iterNode) ifNotNil:[
      (args := args copy) add: itr .
    ].
    ^ args

