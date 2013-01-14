*maglev-runtime
irNode
    | block loopStk cmState |
    labelRedo := nil . "to allow repeating AST to IR transform"
    [ loopStk := (cmState:= RubyCompilerState current) loopStack .
      block := scope irNewBlockNode.
      self ir: block .
      loopStk push: self .
      self useScope: scope during:[
        self buildArgumentsOn: block .
        scope buildTempsOn: block.
        self buildComplexArgsOn: block .
        bodyNode ifNotNil: [ | lexLev |
          lexLev := cmState lexicalLevel .
          self labelRedo: ( GsComLabelNode new lexLevel: lexLev ).
          labelNext := (GsComLabelNode new lexLevel: lexLev argForValue: true).
          block appendStatement: labelRedo .
          bodyNode irNodeListInto: block .
          block appendStatement: labelNext ; 
               lastSourceOffset: endSrcOfs .
        ]
      ].
     ] ensure:[
       loopStk pop: self
     ]. 
    ^ block

