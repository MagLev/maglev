*maglev-runtime
irNode
  ^ inBoot ifTrue:[ bodyNode irNode ]
          ifFalse:[ bodyNode irBlockPassNode ]

