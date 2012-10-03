*maglev-runtime
load: optionsArray  env: envId
  "Returns the current default context after initializing or loading it."
  | ctx sessTmps |
  ctx := (sessTmps := SessionTemps current) at: #DefaultRubyContext  otherwise: nil.
  ctx ifNil:[
    (ctx := Saved) ifNil:[
       Boolean useRubyClasses: true bootStrap: true .
       RubyCompiler initializeParser . 
       ctx := self new .
       "Saved := ctx . " "save early for debugging bootstrap load only"
       sessTmps installRubyContext: ctx .
       ctx initTopNameSpace: envId reload: false .
       ctx setMagLevOptions: optionsArray ;
         requirePrimitives: envId .
       Saved := ctx .
       self commitTransaction .
       ctx initTransient: envId .
    ] ifNotNil:[
       Boolean useRubyClasses: true bootStrap: false .
       sessTmps installRubyContext: ctx .
       ctx setMagLevOptions: optionsArray .
       RubyCompiler initializeParser .
       ctx initTransient: envId .
    ].
  ] ifNotNil:[
    Boolean useRubyClasses: true bootStrap: false .
  ].
  (true virtualClass == TrueClass and:[ false virtualClass == FalseClass]) ifFalse:[
    self error:'Boolean(C)>>useRubyClasses: failed failed for TrueClass/FalseClass'
  ].
  (true class == Boolean and:[ false class == Boolean]) ifFalse:[
    self error:'Boolean(C)>>useRubyClasses: failed for Boolean'
  ].

  System signalAlmostOutOfMemoryThreshold: 110 . "110% is within about 10% of OOM"
  "deleted ctx initTransient: 2 for env 2 "

  ^ ctx

