*maglev-ast
astAnalyzeArgs
  | args |
  args := varNode . 
  args ifNotNil:[
    args == 0 ifTrue:[  
      "zeroDeclaredArgs" 
      "not sure yet where RubyParser produces this case"
       varNode := nil .
       zeroDeclaredArgs := true . 
    ] ifFalse:[ 
      args isSingleIterArg ifTrue:[ 
        " a single DAsgnNode, ok as is"
      ] ifFalse:[ 
        args hasInnerParAssign ifTrue:[
          multArgsNode := args .
          (args := RubyParAsgnStarNode _basicNew) 
          left: { (RubyDAsgnNode _basicNew) name:  #':blkArg1' . 
                  (RubyDAsgnNode _basicNew) name: #':blkArgStar2' }  
        ].
      ].
      varNode := args .  
      self setIsBlockArg: args .
    ]
  ]

