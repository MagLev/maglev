*maglev-runtime
selectorFor: conditionFirstBool
      "maybe ruby_selector_suffix dependent"
 
  ^ conditionFirstBool ifTrue:[ #whileTrue ] ifFalse:[ #untilFalse ]

