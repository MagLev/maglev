converting
adjustForLhsSplat
 
   node class == RubyNilNode ifTrue:[ | n |
	  (n := RubyArrayNode _basicNew ) list: { node  } .
	  node := n .
   ].
   ^ self

