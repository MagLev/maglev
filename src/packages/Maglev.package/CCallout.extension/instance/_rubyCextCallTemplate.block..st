*maglev-cextensions
_rubyCextCallTemplate: argsArray block: aBlock
  | inst |
  inst := #CCalloutInstanceGoesHere .
  ^ inst callCextension: self with: argsArray block: aBlock ex: nil 

