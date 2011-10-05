
set class RubyUnboundMeth
category: '*maglev-runtime'
method:
arity
  "a ruby primitive"
  | n |
  (n := arity) ifNotNil:[  
      n >= 0 ifTrue:[ ^ n ].
  ].
  ^ (self nonBridgeGsMethod: 1"__callerEnvId") rubyArity  .

%


set class RubyUnboundMeth
category: '*maglev-runtime'
method:
bind: anObject
  "a ruby primitive.
   code in UnboundMethod.rb  checks kindOf ... "
  | m |
  (m := RubyMeth _basicNew) .
  m method: gsmeth env: 1"__callerEnvId" selPrefix: selPrefix ;
        bridge: execBridge ; object: anObject .
  ^ m

%


set class RubyUnboundMeth
category: '*maglev-runtime'
method:
bridge: aBridgeMeth
  execBridge := aBridgeMeth

%


set class RubyUnboundMeth
category: 'as yet unclassified'
method:
homeClass
  ^ gsmeth homeMethod inClass

%


set class RubyUnboundMeth
category: '*maglev-runtime'
method:
method: aGsNMethod env: envId selPrefix: rubyName
  "argument should usually be the method or bridge method
    with selector suffix  '#0*&'  "
      "ruby_selector_suffix dependent"
  aGsNMethod isRubyBridgeMethod ifTrue:[
    NameError signal:'expected a non-bridge method for ',  aGsNMethod selector .
  ].
  gsmeth := aGsNMethod .
  arity := aGsNMethod rubyArity .
  rubyName _isSymbol ifTrue:[
    selPrefix := rubyName asString immediateInvariant
  ] ifFalse:[
    rubyName isInvariant ifTrue:[ selPrefix := rubyName ]
          ifFalse:[ selPrefix := rubyName copy immediateInvariant ]
  ].

%


set class RubyUnboundMeth
category: '*maglev-runtime'
method:
nonBridgeGsMethod: envId
  |  m prefix  |
  (m := nonBridgeMeth ) ifNotNil:[ ^ m ].
  m := self _nonBridgeGsMethod: envId .
  m ifNotNil: [ ^ m ].
  prefix := gsmeth selector rubySelectorPrefix .
  NameError signal:'cannot find a non-bridge method for ''' , prefix , ''' in Class ''',
                       (gsmeth inClass @ruby1:name ) , '''' .
  

%


set class RubyUnboundMeth
category: '*maglev-runtime'
method:
_nonBridgeGsMethod: envId
  | cls m prefix  masks |
  (m := gsmeth) isRubyBridgeMethod ifFalse:[ ^ m ].
  m isMethodForBlock ifTrue:[
     NameError signal:'error, method is for a block'
  ].

  cls := m inClass .
  cls ifNil:[ NameError signal:'cannot determine non-bridge method for anonymous method'].
  prefix := m selector rubySelectorPrefixSymbol .

  (cls rubySelectorsWithPrefix: prefix env: envId) do: [ :eaSelector | | cm |
    cm := cls compiledMethodAt: eaSelector rubyEnv: envId .
    (cm == nil or:[ cm _isSmallInteger]) ifFalse:[  cm isRubyBridgeMethod ifFalse:[ ^ cm ]].    
  ] .
  ^ nil 

%


set class RubyUnboundMeth
category: '*maglev-runtime'
method:
_nonBridgeMeth
  "a ruby primitive"
  ^ self _nonBridgeGsMethod: 1"__callerEnvId" 

%

