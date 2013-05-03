*maglev-runtime
rubyAutoload: aName file: fileToLoad
  | pns ofs envId assoc |
  "a ruby primitive.
   Register fileToLoad as the file to require when aSymbolName
   is referenced in the current name space."

  envId := 1 .
  (methDicts ~~ nil) ifTrue: [
    ofs := envId * 4 + 2 .
    pns := methDicts atOrNil: ofs ]
  ifFalse:[
    pns := nil.
  ].
  "there is no pns, or there is no persisted autoload assoc"
  ((pns == nil) or: [(assoc := pns _at: aName otherwise: nil) == nil]) ifTrue: [
    ^ (self transientNameSpaceForStore: envId"__callerEnvId") 
      rubyAutoload: aName asSymbol file: fileToLoad ]
  ifFalse:[
    (self transientNameSpaceForStore: envId"__callerEnvId") addAssociation: assoc copy.
    ^ nil ]

