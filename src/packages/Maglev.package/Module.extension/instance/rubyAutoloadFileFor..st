*maglev-runtime
rubyAutoloadFileFor: aSymOrString
  "a ruby primitive.
  Return the name of the file registered for autoloading of aSymOrString."
  | assoc ns |
  ns := self nameSpace: 1"__callerEnvId" .
  ns ifNotNil:[
    assoc := ns resolveConstant: aSymOrString asSymbol .
    assoc ifNotNil: [
      (assoc isKindOf: RubyAutoloadAssociation) ifTrue: [ ^ assoc fileName ]
    ] .
  ].
  ^ nil .

