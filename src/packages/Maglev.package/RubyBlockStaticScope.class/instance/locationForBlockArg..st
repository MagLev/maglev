*maglev-ast
locationForBlockArg: nameSym
  | vInfo |
  vInfo := self _findVariable: nameSym .
  vInfo ifNotNil:[ "allow duplicate block arg names, Trac 798" | hiddenSym |
    vInfo kind ~~ #blockArg ifTrue:[
      self error:'argument  ', nameSym , '  already defined'
    ].
    hiddenSym := nameSym . 
    [ hiddenSym := ( hiddenSym , '#') asSymbol .
      (self _findVariable: hiddenSym ) == nil
    ] untilTrue .
    vInfo key: hiddenSym .  "hide the previous definition"
  ].
  vInfo := self _addVarInfo: RubyScopeVarInfo name: nameSym kind: #blockArg .
  TraceLocals >= 1 ifTrue:[  self trace:'added ' , nameSym , ' to blockArgs  ' ].

  ^ RubyVarLocation _basicNew varInfo: vInfo depth: 0 scope: self .

