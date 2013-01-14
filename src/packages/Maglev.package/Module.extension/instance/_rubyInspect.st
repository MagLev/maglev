*maglev-runtime
_rubyInspect
  "a ruby primitive"
  | str |
  str := self rubyFullName: 1"__callerEnvId" .
  str size ~~ 0 ifTrue:[ ^ str ].
  str := '#<Module:0x' copy .
  str addAll: self asOop hex ;
     add: $> . 
  ^ str 

