*maglev-ast
irBlockNodeInline: parentNode
    ^ parentNode newInlineBlock:
	    [ :block |
		  block appendStatement: GsComLiteralNode newNil .
		  block
		].

