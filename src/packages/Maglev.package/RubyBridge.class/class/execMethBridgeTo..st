*maglev-runtime
execMethBridgeTo: aMethod
      "ruby_selector_suffix dependent"
  | br toSuff toSel ch optArgsDescr argsDescrInt toNargs |
  toSel := aMethod selector .
  toSuff :=  toSel suffixIfRubySelector .
  toSuff = '#0*&' ifTrue:[ ^ nil "no bridge needed" ].

  optArgsDescr := aMethod rubyOptArgsBits .
  toNargs := aMethod numArgs .
  argsDescrInt := 0 .
  optArgsDescr ~~ 0 ifTrue:[ | idx |
    idx := 1 .
    [ (optArgsDescr bitAt: idx) == 0 ] whileTrue:[ idx := idx + 1 ]. 
    argsDescrInt := argsDescrInt bitOr:(  16r100 bitShift: idx - 1 ) .
  ].
  toSuff ifNotNil:[
    (ch := toSuff at: 4) == $& ifTrue:[ 
       toNargs := toNargs - 1 .
       argsDescrInt := argsDescrInt bitOr: 16r100000 .
    ]. 
    (ch := toSuff at: 3) == $* ifTrue:[ 
       toNargs := toNargs - 1 .
       argsDescrInt := argsDescrInt bitOr: 16r10000
    ].
  ].
  argsDescrInt := argsDescrInt bitOr: (toNargs bitAnd: 16rFF) .
  br := ((BridgeOptions at: 1) at: 16) copy .
  br setTo: toSel suffix: toSuff argsDesc: argsDescrInt primKind: 0 .
  br from: #'call#0*&' .  "actually _executeInContext:nonBridgeMeth:star:block: "
  ^ br execMethodBridge: optArgsDescr env: 1 .

