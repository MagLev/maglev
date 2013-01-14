*maglev-runtime
_compileCaller: rubyName In: aModule enums: eArray
  "A ruby primitive.
   Install a copy of the method CCallout>>_rubyToCcallTemplate:  ,
   which will call the C function specified by the receiver.
   as a module method with name rubyName in aModule.
   eArray is a Ruby Array   [ FFI::Enums , argEnums , retEnum ] "
      "ruby_selector_suffix dependent"

  | envId cmstr argEnums retEnum syms enums templateSel suffix  
      cm litOfs lit callerSel cls  |
  envId := 1"__callerEnvId" .
  cmstr := '"  CALLING:
     ' copy .
  cmstr add: self signatureString ; add: ' "' .
  argEnums := eArray at: 2 .
  retEnum := eArray at: 3 .
  argEnums ifNil:[ retEnum ifNil:[ syms := #( _rubyToCcallTemplate: ) . enums := #() ] 
              ifNotNil:[ syms := #( _rubyToCcallTemplateEnumresult:  CCalloutResultEnumGoesHere )  .
                 enums := { retEnum } ]]
        ifNotNil:[ retEnum ifNil:[ syms := #( _rubyToCcallTemplateEnumargs: CCalloutEnumsInfoGoesHere )  .
                                 enums := { argEnums } ]
                ifNotNil:[ syms := #( _rubyToCcallTemplateEnumargs_results: CCalloutEnumsInfoGoesHere 
                    CCalloutResultEnumGoesHere ) .
               enums := { argEnums . retEnum }] ].
  templateSel := syms at: 1 .
  self _lastArgTypeIsCallback ifTrue:[
    templateSel := (templateSel, 'ampersand:') asSymbol .
    suffix := '#0*&' .
  ] ifFalse:[
    suffix := '#0*_' 
  ].
  "---- was workBlk"
  cm := (CCallout compiledMethodAt: templateSel )
      _copyForClass: aModule aliasFrom: nil to: nil
     comment: cmstr .
  litOfs := cm literalsOffset .
  (lit := cm basicAt: litOfs ) == #CCalloutInstanceGoesHere ifFalse:[
    self error:'inconsistent literals, #CCalloutInstanceGoesHere not found'
  ].
  cm at: litOfs put: self  .
  enums size == 0 ifFalse:[
    2 to: syms size do:[:n | | ofs aSym |
  lit := cm basicAt: (ofs := litOfs + n - 1) .
  lit == (aSym := syms at: n) ifFalse:[
    self error:'inconsistent literals, ' ,  aSym , '  not found'
  ].
  cm at: ofs put: (enums at: n - 1)
    ].
  ].
  cm immediateInvariant .
  (callerSel := rubyName copy)  addAll: suffix .
  callerSel := callerSel asSymbol .
  aModule is_aModule ifTrue:[ cls := aModule moduleMethodsModule ]
      ifFalse:[ cls := aModule class ].
  cls addRubySelector: callerSel method: cm env: envId  .
  RubyBridge installBridgesFor: callerSel in: cls argsDescr: nil
      optArgs: 0 protection: 0 primKind: 0 env: envId .
  "----"

  ^ cm

