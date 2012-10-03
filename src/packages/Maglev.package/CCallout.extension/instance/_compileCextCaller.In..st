*maglev-cextensions
_compileCextCaller: rubyName In: aModule
  | envId templateSel cmstr cm litOfs lit callerSel |
  envId := 1 .
    cmstr := '"  CALLING:
     ' copy .
  cmstr add: self signatureString ; add: ' "' .
  templateSel := #_rubyCextCallTemplate:block: .
  cm := (CCallout compiledMethodAt: templateSel )
      _copyForClass: aModule aliasFrom: nil to: nil
     comment: cmstr .
  litOfs := cm literalsOffset .
  (lit := cm basicAt: litOfs ) == #CCalloutInstanceGoesHere ifFalse:[
    self error:'inconsistent literals, #CCalloutInstanceGoesHere not found'
  ].
  cm at: litOfs put: self  .
  cm immediateInvariant .
  callerSel := rubyName _asSymbolWithRubySuffix: 16r3 " #0*& " .
  aModule addRubySelector: callerSel method: cm env: envId  .
  RubyBridge installBridgesFor: callerSel in: aModule argsDescr: nil
       optArgs: 0 protection: 0 primKind: 0 env: envId .
  ^ cm

