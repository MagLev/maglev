*maglev-runtime
rubyKernelAutoloadFileFor:  aSymOrString
  "a ruby primitive.
  Return the name of the file registered for autoloading of aSymOrString.
  see also Module>>rubyAutoloadFileFor:"
  | assoc ns |
  ns := Object nameSpace: 1"__callerEnvId" .
  assoc := ns resolveConstant: aSymOrString asSymbol .
  assoc ifNotNil: [
    (assoc isKindOf: RubyAutoloadAssociation) ifTrue: [ ^ assoc fileName ] .
  ] .
  ^ nil .

