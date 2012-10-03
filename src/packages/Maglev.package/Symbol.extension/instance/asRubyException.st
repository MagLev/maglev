*maglev-runtime
asRubyException
  "a ruby primitive"
  | assoc val envId | 
  assoc := Object rubyConstAssociationAtOrNil: self env: (envId := 1"__callerEnvId").
  assoc isDefined ifTrue:[
    (val := assoc _value ) _isExceptionClass ifTrue:[ ^ val ].
    ^ ArgumentTypeError signal:'expected an Exception or name of an Exception'
  ] ifFalse:[
    ^ Object @ruby1:const_missing: self 
  ]

