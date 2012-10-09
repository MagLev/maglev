performing
__rubyPerformWrapperBlockFor: aBlock
	"Creates a block to pass to the ruby invocation, that wraps its arguments in RubyWrappers and continues"
	aBlock numArgs = 0 ifTrue: [^ aBlock].
	aBlock numArgs = 1 ifTrue: [^ [:t1 | aBlock value: (RubyWrapper on: t1)]].
	aBlock numArgs = 2 ifTrue: [^ [:t1 :t2 | aBlock
											value: (RubyWrapper on: t1)
											value: (RubyWrapper on: t2)]].
	aBlock numArgs = 3 ifTrue: [^ [:t1 :t2 :t3 | aBlock
											value: (RubyWrapper on: t1)
											value: (RubyWrapper on: t2)
											value: (RubyWrapper on: t3)]].
	aBlock numArgs = 4 ifTrue: [^ [:t1 :t2 :t3 :t4 | aBlock
											value: (RubyWrapper on: t1)
											value: (RubyWrapper on: t2)
											value: (RubyWrapper on: t3)
											value: (RubyWrapper on: t4)]].
	aBlock numArgs = 5 ifTrue: [^ [:t1 :t2 :t3 :t4 :t5 | aBlock
											value: (RubyWrapper on: t1)
											value: (RubyWrapper on: t2)
											value: (RubyWrapper on: t3)
											value: (RubyWrapper on: t4)
											value: (RubyWrapper on: t5)]].
	^ self error: 'Only blocks with up to 5 arguments are supported by the RubyService'


