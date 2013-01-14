*maglev-runtime
sequenceExpand: skipFirst

"Return a copy of self with all sequences expanded.  A sequence is two characters separated by $-, e.g., a-z.
If skipFirst is true, then skip the first character."

| expanded index limit rcvrSize chCls  |

expanded := String new.
(rcvrSize := self size) == 0 ifTrue:[ ^ expanded ].

index :=  skipFirst ifTrue:[ 2 ] ifFalse:[ 1].
limit := rcvrSize - 2 .
chCls := Character .
[ index <= limit  ] whileTrue:[
	(self at: index + 1) == $- 
	  ifTrue:[ "expand character sequence: e.g., a-z"
		(self codePointAt: index) to: (self codePointAt: (index + 2))
		     do:[:i| expanded add: (chCls withValue: i)].
		index := index + 2 ]
	ifFalse:[ expanded add: (self at: index) ].
	index := index + 1 
] .
[ index <= rcvrSize ] whileTrue:[
	expanded add: (self at: index) .
	index := index + 1 
] .
^ expanded.

