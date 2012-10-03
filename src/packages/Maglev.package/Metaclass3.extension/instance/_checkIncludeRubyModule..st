*maglev-runtime
_checkIncludeRubyModule: aModule 
  "a ruby primitive"
  aModule class == Module ifFalse:[
    ((aModule isKindOf: Module) and:[ (aModule isKindOf: Metaclass3) not]) ifFalse:[
      ArgumentTypeError signal:'argument to include is not a Module'
    ].
  ].
  aModule == self ifTrue:[
    ArgumentError signal:'cannot include a module in itself'.
  ].
  ^ true

