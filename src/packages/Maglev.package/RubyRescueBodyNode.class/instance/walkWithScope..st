(as yet unclassified)
walkWithScope: aScope
  | exn |
  (exn := exceptionNodes) ifNil:[ 
    (defaultNode := RubyColon2Node newForIr name: #StandardError ) walkWithScope: aScope
  ] ifNotNil:[
    exn walkWithScope: aScope .
  ].
  bodyNode walkWithScope: aScope .
  nextRescueBody walkWithScope: aScope .

