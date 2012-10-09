*maglev-runtime
reimplementationAllowed: aSymbol node: aNode cst: aRubyCompilerState
      "ruby_selector_suffix dependent"
  (self class reimplementationAllowed: aSymbol for: currentClass 
                  cst: aRubyCompilerState ) ifFalse:[ | msg |
     msg := 'reimplementation of  ' , aSymbol , '  not allowed, near ' ,
                    aNode sourcePositionAsString .
     aRubyCompilerState installingPrims ifTrue:[
       Error signal: msg .
     ].
     (SessionTemps current at: #MAGLEV_parseWarn otherwise: false) ifTrue: [
       GsFile gciLogServer: 'WARNING ' , msg .
       "(aSymbol at:1 equals:'__send') ifTrue:[ self pause ]."
     ].
     ^ false
  ].
  ^ true

