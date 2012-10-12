initialization
exceptionKindOfBlk

	^ [:expr | RubyCallNode
				s_a: (RubyGlobalLastException s_a: #'$!')
				b: #'_kind_of?'
				c: (RubyRpCallArgs s_a: expr)].
