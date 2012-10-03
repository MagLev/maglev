*maglev-runtime
bodyNode: aNode source:  fileSourceStr
  | bdy |
  (bdy := RubyClassBodyNode _basicNew)   
     bodyNode: aNode ;
     source: fileSourceStr "nil if using MRI parse server" ;
     lineBias: lineBias ;
     position: position .
  self class == RubyClassNode ifTrue:[ bdy classNode: self ].
  bodyNode := bdy 

