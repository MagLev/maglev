initialization
initSplatRescue: splatNode with: body and: nextRescue at: srcOfs
	"This uses a lot of stuff. Can only be used outside bootstrap!"
	| condition array |
	array := RubyCallNode
				s_a: (RubyColon2Node sym: #Array srcOffset: srcOfs)
				b: #'__withall'
				c: (RubyRpCallArgs s_a: splatNode node).
	condition := RubyCallNode
					s_a: array
					b: #'__anySatisfyCaseLeaf'
					c: (RubyRpCallArgs
							s_a: (RubyGlobalLastException s_a: #'$!')).
	self initRescue: condition with: body and: nextRescue at: srcOfs.
