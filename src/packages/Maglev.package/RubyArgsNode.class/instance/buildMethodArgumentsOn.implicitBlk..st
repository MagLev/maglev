*maglev-runtime
buildMethodArgumentsOn: irNode implicitBlk: hasBlockRef 
   "Returns the one-based arg number of last fixed non-optional argument"
      "ruby_selector_suffix dependent"
    | n myScop nRequired nFixed fixedCp descrInt optArgsBits optA suffix |
    n := 0.
    optArgsBits := 0 .
    descrInt := 0 .
    suffix := '#0__' copy .
    myScop := self currentScope .
    1 to: (nRequired := arguments size) do:[:m |
      irNode appendArg: (myScop argLeafAtIndex: m) .
    ].
    nFixed := nRequired .
    n := nRequired .
    (optA := optArgs) ifNotNil:[ | optSz |
      optSz := optA list size .
      optSz > 0 ifTrue:[
        descrInt := descrInt bitOr: (n + 1) << 8 . " one-based number of first optional arg"
        1 to: optSz do: [:m |
          optArgsBits := optArgsBits bitOr: ( 1 << n  ) ."zero based bitnum"
          n := n + 1 . 
          irNode appendArg: (myScop argLeafAtIndex: n) .
        ].
        nFixed := nFixed + optSz .
      ].
    ].
    fixedCp := 48 + nFixed . " $0 + "  "also enforced in RubyArgsNode::add_arg"
    fixedCp > 255 ifTrue:[ self error:'too many fixed args'].
    suffix codePointAt: 2 put: fixedCp .

    descrInt := descrInt bitOr: n .
    restArgNode ifNotNil:[
        descrInt := descrInt bitOr: 16r10000 .
        n := n + 1 .       suffix at: 3 put: $*  .
        irNode appendArg: (myScop argLeafAtIndex: n)
        ].
    blockArgNode ifNotNil:[ | baLeaf | 
        descrInt := descrInt bitOr: 16r100000 .  
        n := n + 1 .       suffix at: 4 put:  $&  .
        irNode appendArg: (myScop blockArgLeaf: (myScop argLeafAtIndex: n )).
    ] ifNil:[  hasBlockRef ifTrue:[  
           irNode appendArg: (myScop blockArgLeaf: nil )  .
           descrInt := descrInt bitOr: 16r100000 .
           suffix at: 4 put:  $& .
    ].].
    n > 255 ifTrue:[ self error:'too many (>256) args in method definition'].
    argsDescrInt := descrInt . 
    myScop ~~ self currentScope ifTrue:[ self error:'scope changed' ].  
    selectorSuffix := suffix .
    irNode  optArgsBits: optArgsBits .
    ^ nFixed

