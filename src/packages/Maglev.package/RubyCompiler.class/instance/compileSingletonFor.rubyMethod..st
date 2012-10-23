*maglev-runtime
compileSingletonFor: anObject rubyMethod: aNode
  "called from generated code.
   two use cases, 1)  inside of a class;...end  we are at a def self.foo;...end
                    2)  def anObj.foo;...end "
  | cls selPrefix singletonCls  envId |
  envId := 1"__callerEnvId" .
  anObject class == Module ifTrue:[
    cls := anObject moduleMethodsModuleOrNil ifNil:[  anObject _rubyModuleIncludeSelfEnv: envId 
].
  ] ifFalse:[
    anObject isBehavior ifTrue:[
      cls := anObject virtualClass "compiling a class method"
    ] ifFalse:[
      anObject _isNumber ifTrue:[
        ArgumentTypeError signal:'singleton method not allowed on a Numeric'
      ].
      cls := anObject _singletonClassFor: envId .
    ].
  ].
  singletonCls := cls .
  selPrefix := self compileSelector: nil inClass: cls rubyMethod: aNode
                      env: envId .
  selPrefix ifNotNil:[
    installingPrims ifFalse:[
     "ruby_selector_suffix dependent , use perform to bypass protection"
      singletonCls ifNotNil:[
         anObject with: selPrefix perform: #'singleton_method_added#1__' env: 1 .
      ] ifNil:[
         cls with: selPrefix perform: #'method_added#1__' env: 1 .
      ].
    ].
  ].

