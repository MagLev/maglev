*maglev-runtime
rubyLiteralClass
	(self at: 1) == $/
		ifTrue: [ ^ RubyRegexpNode ].
		
	^ (self includesSubString: '..' )
		ifTrue:[ RubyDotNode ]
		ifFalse:[ RubyAbstractNumberNode ]

