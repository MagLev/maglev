
set class RubyConstDeclNode class
category: '*maglev-ast'
method:
colon2: left name: name_tok
  | c2n res |
  c2n := RubyColon2Node s_a: left b: name_tok .
  (res := self _basicNew)
     constNode: c2n ;
     position: name_tok src_offset . "valueNode is nil"
  ^ res

%


set class RubyConstDeclNode class
category: '*maglev-ast'
method:
colon3: name_tok
  | c3n res |
  c3n := RubyColon3Node s_a: name_tok .
  (res := self _basicNew)
     constNode: c3n ;
     position: name_tok src_offset . "valueNode is nil"
  ^ res

%


set class RubyConstDeclNode class
category: '*maglev-ast'
method:
sym: sym ofs: srcOfs val: val
  | c2n res |
  (c2n := RubyColon2Node _basicNew) 
     name: sym ; position: srcOfs .
  (res := self _basicNew) 
     constNode: c2n ; valueNode: val  ; position: srcOfs .
  ^ res

%


set class RubyConstDeclNode
category: '*maglev-ast'
method:
as_accessor
  ^ constNode copy "Fix Trac 588 ?"

%


set class RubyConstDeclNode
category: '(as yet unclassified)'
method:
childrenForMatch
  | res |
  (res := super childrenForMatch) addLast: constNode .
  ^ res

%


set class RubyConstDeclNode
category: 'accessing'
method:
constNode

	 ^ constNode

%


set class RubyConstDeclNode
category: 'accessing'
method:
constNode: aINameNode
	constNode := aINameNode

%


set class RubyConstDeclNode
category: '(as yet unclassified)'
method:
definedQkind
   ^  #constant

%


set class RubyConstDeclNode
category: 'as yet unclassified'
method:
irAssignmentNode: srcVarNode 
  isDynamic ifFalse:[ ^ super irAssignmentNode: srcVarNode ].
  ^ constNode irForDynamicCDecl: srcVarNode

%


set class RubyConstDeclNode
category: '(as yet unclassified)'
method:
irLeaf
  isDynamic ifTrue:[ self error:'illegal irLeaf for dynamic ConstDecl'].
  globalAssoc ifNil:[ self error:'unresolved global during IR'].
  ^ self irLitVarLeaf: globalAssoc 

%


set class RubyConstDeclNode
category: '*maglev-runtime'
method:
irNode
  "deleted isDynamic optimization, not compatible with reload prims "
  ^ constNode irForDynamicCDecl:  valueNode irEvaluatedBlockNode 

%


set class RubyConstDeclNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  |  leftDyn constn |
  leftDyn := (constn := constNode) walkWithScopeForCdecl: aScope .  
  (leftDyn == 0 and:[ aScope inBootstrap]) ifTrue:[ | aPath pSize |
    aPath := constn pathArray .
    (pSize := aPath size) == 1 ifTrue:[
      isDynamic := false .
      globalAssoc := aScope nameSpace bootAddConstAssociation: (aPath at: 1)
												env: (RubyCompilerState current envId) . 
    ] ifFalse:[
       pSize== 0 ifTrue:[ self error:'ConstDeclNode has empty path'].
       isDynamic := true .
    ]
  ] ifFalse:[
    isDynamic := true 
  ].
  super walkWithScope: aScope . "for valueNode"

%


set class RubyConstDeclNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:cdecl, ', constNode _inspect, ', ', valueNode _inspect , $]

%

