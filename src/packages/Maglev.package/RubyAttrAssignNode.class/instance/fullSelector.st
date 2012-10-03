*maglev-runtime
fullSelector
   "caller has already computed irArgNodes and numIrArgs for the receiver"
   | sel  numIrArgs  hasSplat num_colons hasTooMany mask splatPos |
   sel := self selector .
   numIrArgs := irArgNodes size .
   num_colons :=  numIrArgs  .
   splatPos := splatPosition .
   (hasSplat := splatPos ~~ 0 ) ifTrue:[ num_colons := splatPos - 1 ].
   (hasTooMany := num_colons > 3"maxColons") ifTrue:[ 
      num_colons := 3 .  
   ].
   mask := num_colons * 4 . 
   hasTooMany ifTrue:[ mask := mask bitOr: 16r2  ]
              ifFalse:[ hasSplat ifTrue: [ mask := mask bitOr: 16r2 ]].
   fixedArgCount := num_colons .
   ^ sel _asSymbolWithRubySuffix: mask 

