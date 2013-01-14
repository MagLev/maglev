*maglev-runtime
_valueFor: aSymbol inClass: aClass env: envId 
      "ruby_selector_suffix dependent"
  | assoc | 
  self isCommitted ifTrue:[ | tns |
    "the committed copy will have the block but no value"
    tns := aClass transientNameSpaceForStore: envId .
    assoc := tns _at: aSymbol otherwise: nil.
    assoc ifNil:[
      (assoc := RubyTransientConstantAssociation new) 
         key: aSymbol; 
         block: block class: aClass env: envId .
      tns addTransientAssociation: assoc .
    ].
  ] ifFalse:[
    assoc := self
  ].
  assoc _isDefined ifNil:[ 
    ^ assoc _value:(
      aClass with: block perform: #'__transient_const_evaluate#0_&' env: envId )
  ] ifNotNil:[
    ^ assoc _valueNoAction
  ]

