*maglev-runtime
fullSelector: isStSend
  | fs |
  (fs := forceSelector) ifNotNil:[ 
    lastArgIsBlk := self hasBlockArg .
    fixedArgCount := 1 .
  ] ifNil:[
    fs := super fullSelector: isStSend .   
  ].
  lastArgIsBlk ifTrue:[  self error: 'unexpected lastArgIsBlk for yield'  ]. 
  ^ fs 

