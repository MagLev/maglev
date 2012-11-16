(as yet unclassified)
walkWithScope: aScope
  | cnt fixCls |
  cnt := 0 .
  beginNode class == (fixCls := RubyFixnumNode) ifTrue:[ cnt := 1 ].
  endNode class == fixCls ifTrue:[ cnt := cnt + 1 ].
  fixNumCount := cnt .
  super walkWithScope: aScope   "beginNode is receiver, endNode is args "

