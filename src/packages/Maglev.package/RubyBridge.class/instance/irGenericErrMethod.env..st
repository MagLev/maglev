*maglev-runtime
irGenericErrMethod: fromSelector env: envId
  | node isErr |
  (node := GsComMethNode newRuby)
     environment: envId ;
     selector: fromSelector ;
     setRubyBridge ;
     source: 
'<a Ruby bridge method>
  ' .
  self buildMethodArgumentsOn: node .
  isErr := self buildBodyOn: node argsDescr: 0"default optArgDescr" .
    ^ isErr ifTrue:[ node ] ifFalse:[  nil ].
    

