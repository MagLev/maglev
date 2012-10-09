*maglev-runtime
_rubyLoop2: aBlock
  "a ruby primitive, conforming to Object>>_rubyEach1: "
    aBlock ifNil:[ CannotReturn signal:'no block given'].
    [
      [
        [ true ] whileTrue:[    aBlock @ruby1:value ].
      ] onException: RubyStopIterationError do: [:ex |   "added for 1.8.7"
        ^ self "Ruby StopIteration caught, terminate iteraton"
      ]
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1)  ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry - restart the enumeration"
      ]
    ] .
    ^ self

