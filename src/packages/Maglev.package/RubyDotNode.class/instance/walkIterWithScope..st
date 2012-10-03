(as yet unclassified)
walkIterWithScope: aScope
  | cnt fixCls |
  cnt := 0 .
  beginNode class == (fixCls := RubyFixnumNode) ifTrue:[ cnt := 1 ]
                                ifFalse:[ beginTmp := aScope newEvaluationTemp ].
  endNode class == fixCls ifTrue:[ cnt := cnt + 1 ]
                            ifFalse:[ endTmp := aScope newEvaluationTemp ].
  fixNumCount := cnt .
  isInline := cnt > 0 .
  super walkWithScope: aScope   "beginNode is receiver, endNode is args "

