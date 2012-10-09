as yet unclassified
irNode
  |  goto val irLabel targLev  cm loop |
  loop := (cm := RubyCompilerState current) topLoop .
  irLabel := self irGotoTarget: loop  .
  irLabel ifNotNil:[
    targLev := irLabel lexLevel . 
    (cm lexLevelIsInlineWithin: targLev) ifTrue:[
	  (goto := self irGotoNodeFrom: loop to: irLabel) 
       argNode: self irGotoValueNode .
    ].
  ].
  goto ifNil:[ goto := self nonInlineIrNode ].
  self ir: goto . 
  ^ goto

