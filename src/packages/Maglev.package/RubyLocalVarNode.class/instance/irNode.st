*maglev-runtime
irNode 
      "ruby_selector_suffix dependent"
  | loc |
  loc := location .
  useToProc == 1 ifTrue:[ | lf tmpLf toprocSnd asgn tst |
    lf := loc leaf .
    tmpLf :=  loc varInfo toProcInfo leaf .
    (toprocSnd := GsComSendNode new)
       rcvr:( GsComVariableNode new leaf: lf );
       rubySelector: #'__to_proc#0__'  .
    asgn := GsComAssignmentNode _basicNew dest: tmpLf source: toprocSnd .
    (tst := GsComSendNode new)
       rcvr:( GsComVariableNode new leaf: tmpLf );
       stSelector: #ifNil: ;
       appendArgument:( self irInlineBlockIr: asgn ) ;
       optimize .
    self ir: toprocSnd ; ir: tst .
    ^ tst
  ] ifFalse:[  
    loc leaf ifNotNil:[ :lf |
      ^ self ir: (GsComVariableNode new leaf: lf)
    ].
    ^ loc irNode "fetch from var in an eval binding, trac 829"
  ]

