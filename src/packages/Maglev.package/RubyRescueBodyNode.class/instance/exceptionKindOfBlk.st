initialization
exceptionKindOfBlk

	^ [:expr |
		RubyCallNode
				s_a: expr
				b: #===
				c: (RubyRpCallArgs s_a: (RubyGlobalLastException s_a: #'$!'))].
