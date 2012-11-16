*maglev-runtime
lineForOffset: anOffset
  source ifNotNil:[ ^ source lineForOffset: anOffset ].
  ^ startLine "ignore anOffset"

