*maglev-runtime
rubyAutoload: sym file: fileToLoad
  | assoc |
  fileToLoad size == 0 ifTrue:[ 
     ArgumentError signal:'empty filename passed to autoload'
  ].
  (sym size == 0 or:[ (sym at: 1) isUppercase not ]) ifTrue:[
     NameError signal:'invalid constant name passed to autoload'
  ].
  assoc := self resolveConstant: sym .
  assoc ifNotNil:[
    assoc isDefined ifNotNil:[ ^ nil ].
    assoc class == RubyAutoloadAssociation ifFalse:[
      "remove key->NameSpace assoc created during walkWithScope for a Module"
      self removeKey: sym .
      assoc := nil .
    ].
  ].
  assoc ifNil:[ self addPersistentAssociation: 
             (RubyAutoloadAssociation newWithKey: sym file: fileToLoad) .
  ].
  ^ nil .

