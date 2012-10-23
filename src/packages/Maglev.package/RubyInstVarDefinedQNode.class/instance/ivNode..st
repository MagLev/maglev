as yet unclassified
ivNode: instVarNode 
  | accessKind |
  ivNode := instVarNode .
  accessKind := ivNode accessKind .
  accessKind == 0 ifTrue:[ | ivLeaf varOfs  |
	selector :=  #_rubyInstVarDefinedQ:  .
	ivLeaf := instVarNode irLeaf .
	varOfs := ivLeaf varOffset .
	varOfs < 0 ifTrue:[  argNode := instVarNode name asSymbol irLiteralNode "dynamic iv" ]
			   ifFalse:[  argNode := GsComLiteralNode newInteger: varOfs "fixed iv" ].
  ] ifFalse:[
    selector := #rubyInstVarDefined:  .   "instVar in Object or a Behavior"
    argNode := instVarNode name asSymbol irLiteralNode .
  ].

