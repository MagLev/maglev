
set class RubyAttrAssignNode
category: '*maglev-ast'
classmethod:
s_a: rcvr b: name_tok c: args d: ofs

^ self _basicNew init: rcvr sel: name_tok args: args srcOffset: ofs

%


set class RubyAttrAssignNode
category: 'converting'
method:
argNodes
	^ argsNode argNodes

%


set class RubyAttrAssignNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubyAttrAssignNode
category: 'accessing'
method:
argsNode: aNode
	argsNode := aNode

%


set class RubyAttrAssignNode
category: '*maglev-runtime'
method:
buildArgumentsOn: irNode 
    | nodes na  ncolons idx |
    nodes := irArgNodes .
    na := nodes size .
    ncolons := fixedArgCount .  "excludes the splat, if any"
    idx := 1 .
    [ idx <= ncolons ] whileTrue:[
       irNode appendArgument: (nodes at: idx) .
       idx := idx + 1
    ].
    idx <= na ifTrue:[ "args beyond 3 colons or at,after splat"
      | restArray aSend splatPos |
      restArray := GsComArrayBuilderNode new.
      (splatPos := splatPosition) ~~ 0 ifTrue:[
        [ idx < splatPos ] whileTrue:[
           restArray appendElement: (nodes at: idx) .
           idx := idx + 1
        ].
        (aSend := GsComSendNode new) 
           rcvr: restArray ; stSelector: #_rubyAddAll:  ;
           appendArgument: (nodes at: idx) .
        self ir: aSend .
        restArray := aSend .
        idx := idx + 1 .
        [ idx <= na ] whileTrue:[
          (aSend := GsComSendNode new)
             rcvr: restArray ;  stSelector: #_rubyAddLast: ;
             appendArgument: (nodes at: idx) .
          self ir: aSend .
          restArray := aSend .
          idx := idx + 1 .
        ].
      ] ifFalse:[
        [ idx <= na ] whileTrue:[
           restArray appendElement: (nodes at: idx) .
           idx := idx + 1
        ].
      ].
      irNode appendArgument: restArray .
    ].

%


set class RubyAttrAssignNode
category: 'as yet unclassified'
method:
buildBlockArgumentsOn: irNode
  "block_spec.rb  has comments that rubinius does not support this"
  self error: 'assignment to attribute not supported as a block arg'
		

%


set class RubyAttrAssignNode
category: 'as yet unclassified'
method:
definedQkind
  ^ 'assignment'

%


set class RubyAttrAssignNode
category: '*maglev-runtime'
method:
fullSelector
   "caller has already computed irArgNodes and numIrArgs for the receiver"
   | sel  numIrArgs  hasSplat num_colons hasTooMany mask splatPos |
   sel := self selector .
   numIrArgs := irArgNodes size .
   num_colons :=  numIrArgs  .
   splatPos := splatPosition .
   (hasSplat := splatPos ~~ 0 ) ifTrue:[ num_colons := splatPos - 1 ].
   (hasTooMany := num_colons > 3"maxColons") ifTrue:[ 
      num_colons := 3 .  
   ].
   mask := num_colons * 4 . 
   hasTooMany ifTrue:[ mask := mask bitOr: 16r2  ]
              ifFalse:[ hasSplat ifTrue: [ mask := mask bitOr: 16r2 ]].
   fixedArgCount := num_colons .
   ^ sel _asSymbolWithRubySuffix: mask 

%


set class RubyAttrAssignNode
category: '*maglev-ast'
method:
init: rcvr sel: name_tok args: args srcOffset: ofs
| nam |
receiverNode := rcvr .
argsNode := args .
name_tok ifNil:[
  name := #'[]=' .
  position := ofs .  
] ifNotNil:[ | nm |
  (nm := String withAll: name_tok symval asString) add: $= .
  name := nm asSymbol .
  position := name_tok src_offset .
].
^ self

%


set class RubyAttrAssignNode
category: '*maglev-runtime'
method:
irArgNodes
  | res args sz splatPos rhsIr lim |
  args := self argNodes .
  sz := args size . lim := sz .
  res := { } .
  splatPos := 0 .
  "srcIrNode may already be set from masgn ..." 
  (rhsIr := srcIrNode ) ifNil:[ lim := sz - 1 ]. 
  1 to: lim do:[:n| | anArg |
    anArg := args at: n .
    anArg isSplatNode ifTrue:[ splatPos := n ].
    res add: anArg irArgNode .  
  ].
  res add: (rhsIr ifNil:[ (args at: sz) irLocalAsgnValue ]).
  splatPosition := splatPos .
  irArgNodes := res .
  ^ res

%


set class RubyAttrAssignNode
category: 'as yet unclassified'
method:
irAssignmentNode: srcVarNode 
  "used by IR phase of RubyParAsgnNode "			
  srcIrNode ifNotNil:[ self error: 'inconsistent AST tree'].
  srcIrNode := srcVarNode .

  ^ self irNode

%


set class RubyAttrAssignNode
category: '*maglev-runtime'
method:
irNode
  | irArgs na fullSel snd tasgn tmpLeaf |
  irArgs := self irArgNodes .
  na := irArgs size  .
  fullSel := self fullSelector .
  tasgn := GsComAssignmentNode _basicNew dest:( tmpLeaf := resTmp leaf )
                   source: (irArgs at: na)   .
  self ir: tasgn .
  irArgs at: na put: tasgn .
  (snd := GsComSendNode new)  rcvr:  receiverNode irNode ;
       rubySelector: fullSel .
  self buildArgumentsOn: snd .
  self ir: snd . 
  ^ GsComStatementsNode new list: { snd . (GsComVariableNode new leaf: tmpLeaf) }.

%


set class RubyAttrAssignNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ self name = other name

%


set class RubyAttrAssignNode
category: 'as yet unclassified'
method:
isSingleIterArg
  "block_spec.rb  has comments that rubinius does not support this"
  self error: 'assignment to attribute not supported as a block arg'
		

%


set class RubyAttrAssignNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyAttrAssignNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyAttrAssignNode
category: '*maglev-ast'
method:
node_assign_set_rhs: rhs
  | args |
  (args := argsNode) ifNil:[ 
    argsNode :=  RubyRpCallArgs _new: rhs .
  ] ifNotNil:[
    args _append: rhs .
  ].
  ^ self

%


set class RubyAttrAssignNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		printNode: receiverNode;
		nextPutAll: '.', name allButLast, ' = '.
	argsNode printArgsOn: aStream

%


set class RubyAttrAssignNode
category: 'converting'
method:
receiverNode
	^ receiverNode

%


set class RubyAttrAssignNode
category: 'accessing'
method:
receiverNode: aNode
	receiverNode := aNode

%


set class RubyAttrAssignNode
category: 'converting'
method:
selector
	^ name

%


set class RubyAttrAssignNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  resTmp := aScope newEvaluationTemp .
  ^ super walkWithScope: aScope

%


set class RubyAttrAssignNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:attrasgn, ', receiverNode _inspect, ', ',
                 name , ', ', 
                 argsNode _inspect , $]

%

