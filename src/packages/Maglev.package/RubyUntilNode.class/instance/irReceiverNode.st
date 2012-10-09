as yet unclassified
irReceiverNode
  ^  blksInline ifTrue:[ self irBodyNode ]
                ifFalse:[ self irConditionNode ]

