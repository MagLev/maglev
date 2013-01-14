*maglev-runtime
irNode 
      "ruby_selector_suffix dependent"
    |  argIrs  argIrsSiz asgnNodes 
       tmpLeafs  nTmps mName blkSel  argl asz |
    argl := argsList .   "from walkWithScope phase"
    argIrs := Array new:( asz := argl size ).
    1 to: asz do:[:n | argIrs at: n put: ( argl at: n) irNode  ].

    tmpLeafs := evalTmpAssocs copy .     nTmps := tmpLeafs size .
    1 to: nTmps do:[:m | tmpLeafs at: m put:(tmpLeafs at: m) leaf ].
    asgnNodes := Array new: nTmps .
    asgnNodes at: 1 put: ( GsComAssignmentNode _basicNew  dest: ( tmpLeafs at: 1)  
                source: self irReceiverNode ).
    1 to: (argIrsSiz := argIrs size)  do:[ :k | 
       asgnNodes at: k + 2  put:( GsComAssignmentNode _basicNew dest: (tmpLeafs at: k + 2)  
                    source: (argIrs at: k)) .
    ].
    mName := callName .
    mName == #'||' ifTrue:[ blkSel := #or: ] 
                ifFalse:[ mName == #'&&' ifTrue:[ blkSel := #and: ]].
    ^ blkSel ifNotNil:[ self irOpElAsgn: asgnNodes tmps: tmpLeafs blkSel: blkSel ]
             ifNil:[ self irOpElAsgn: asgnNodes tmps: tmpLeafs mName: mName ].

