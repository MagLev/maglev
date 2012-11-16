as yet unclassified
pathForTrace: aPath
  | hm |
  hm := RubyEnv _getenv:'MAGLEV_HOME' .
  (aPath at:1 equals: hm) ifTrue:[ ^ '$MAGLEV_HOME' , ( aPath copyFrom: hm size + 1 to: aPath size)]
                           ifFalse:[ ^ aPath ]

