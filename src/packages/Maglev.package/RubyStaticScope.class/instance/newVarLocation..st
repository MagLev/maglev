*maglev-runtime
newVarLocation: aName 
  | vInfo res encl |
  enclosingScope class == RubyEvalScope ifTrue:[
    res := enclosingScope newVarLocation: aName .
    res ifNotNil:[ ^ res ].
  ].
  vInfo := self _addVarInfo: RubyScopeVarInfo name: aName kind: #normal .
  TraceLocals >= 1 ifTrue:[  self trace:'added ' , aName , ' to variableNames '].
  res := RubyVarLocation _basicNew varInfo: vInfo depth: 0 scope: self .
  ^ res

