*maglev-runtime
walkWithScopeForCdecl: aScope
  "This usage is for a Colon2 node that is lhs of a RubyConstDeclNode.
   returns the leftIsDynamic value "
   | isDyn lft |
   (lft := leftNode) ifNotNil:[
      lft walkWithScope: aScope .
      isDyn := 1 .
   ] ifNil:[ 
      isDyn := 0
   ].
   leftIsDynamic := 0 .
   ^ isDyn 

