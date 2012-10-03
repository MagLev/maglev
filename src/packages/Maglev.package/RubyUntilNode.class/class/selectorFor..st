*maglev-runtime
selectorFor: conditionFirstBool
      "maybe ruby_selector_suffix dependent"
  
  ^ conditionFirstBool ifTrue:[ #whileFalse ] ifFalse:[ #untilTrue ]

