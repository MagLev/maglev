as yet unclassified
irGotoValueNode
  ^ valueNode ifNil:[ GsComLiteralNode newNil ]
            ifNotNil:[ valueNode irLocalAsgnValue ].

