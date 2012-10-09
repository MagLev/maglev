*maglev-runtime
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

