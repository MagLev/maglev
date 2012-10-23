*maglev-runtime
fullSelector: isStSend
    "caller has already computed irArgNodes and numIrArgs for the receiver"
    | sel  numIrArgs  |
    sel := self selector .
                "trc := sel at: 1 equals: 'aruby_selector_prefix'  ."
    numIrArgs := irArgNodes size .
    isStSend ifTrue:[
       fixedArgCount := numIrArgs .
       lastArgIsBlk := false .
       ^ sel asSymbol 
    ] ifFalse:[ |  hasBlk hasRest hasTooMany maxColons num_colons num_args mask  |
        lastArgIsBlk := ( hasBlk := self hasBlockArg)  .
        hasRest := self hasRestArg or:[ self argIsArgsCat ].
        num_colons :=  hasBlk ifTrue:[ numIrArgs - 1 ] ifFalse:[ numIrArgs ] .
        hasRest ifTrue:[ num_colons := num_colons - 1 ].
        maxColons :=  sel == #__perform__se ifTrue:[ 5 "for RubyParser" ] ifFalse:[ 3 ]. 
        (hasTooMany := num_colons > maxColons) ifTrue:[ 
           num_colons := maxColons .  
        ].
        num_args := num_colons .
        num_colons < 0 ifTrue:[ self error:'negative arg count'].
        mask := num_colons * 4 . 
        hasTooMany ifTrue:[ mask := mask bitOr: 16r2  ]
              ifFalse:[ hasRest ifTrue: [ 
                  mask := mask bitOr: 16r2 . num_args := num_args + 1  
              ]].
        hasBlk ifTrue: [  mask := mask bitOr: 16r1  ].
        fixedArgCount := num_args .
        ^ sel _asSymbolWithRubySuffix: mask 
    ].

