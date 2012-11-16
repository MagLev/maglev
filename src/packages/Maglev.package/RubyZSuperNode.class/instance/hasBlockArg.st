as yet unclassified
hasBlockArg
  ^ iterNode ifNotNil:[ true ] ifNil:[ methSelector last == $& ].

