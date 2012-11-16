*maglev-runtime
addIncomingBlock: nameSym
  | vInfo |
  vInfo := self _findVariable: nameSym .
  vInfo ifNotNil:[ self error:'incomingBlockArg already defined'].
  vInfo := self _addVarInfo: RubyScopeIncomingBlockVar name: nameSym kind: #incomingBlockArg .
  vInfo toProcInfo: self newEvaluationTemp boot: inBootstrap .
 
  ^ RubyVarLocation _basicNew varInfo: vInfo depth: 0 scope: self .

