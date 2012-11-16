as yet unclassified
iterNode_forRp: anIterNode
  iterNode ifNotNil:[
     self signalParseError: 'both block arg and actual block given '
  ].
  iterNode := anIterNode

