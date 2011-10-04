
doit
RubyAssignableNode subclass: 'RubyGlobalAsgnNode'
	instVarNames: #( name globalAssoc)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalAsgnNode
removeallmethods
removeallclassmethods

set class RubyGlobalAsgnNode class
category: 'as yet unclassified'
method:
newForIr
  ^ self _basicNew

%


set class RubyGlobalAsgnNode class
category: 'as yet unclassified'
method:
newForRp
  ^ self _basicNew

%


set class RubyGlobalAsgnNode class
category: '*maglev-ast'
method:
s_a: aSymbol b: srcOfs c: val
  | arr cls node sym |
  arr := (SessionTemps current at:#RubyGlobalNodesDict) at: aSymbol otherwise: nil .
  arr ifNotNil:[ cls := arr at: 3 . sym := arr at: 1] 
         ifNil:[ cls := RubyGlobalAsgnNode . sym := aSymbol ].
  (node := cls newForRp )
      name: sym ; valueNode: val ; position: srcOfs .
  ^ node

%


set class RubyGlobalAsgnNode
category: '*maglev-ast'
method:
as_accessor
  ^ RubyGlobalVarNode s_a: name

%


set class RubyGlobalAsgnNode
category: 'as yet unclassified'
method:
buildBlockArgumentsOn: irNode
  "block_spec.rb  has comments that rubinius does not support this"
  self error: 'assignment to global not supported as a block arg'

%


set class RubyGlobalAsgnNode
category: '*maglev-runtime'
method:
irAssignmentNode: srcVarNode
  ^  self _irNodeSrcIr: srcVarNode 

%


set class RubyGlobalAsgnNode
category: '*maglev-runtime'
method:
irNode
  ^ self _irNodeSrcIr: valueNode irEvaluatedBlockNode

%


set class RubyGlobalAsgnNode
category: 'as yet unclassified'
method:
irValidateArg: anIrNode
  ^ anIrNode

%


set class RubyGlobalAsgnNode
category: 'as yet unclassified'
method:
isSingleIterArg
  "block_spec.rb  has comments that rubinius does not support this"
  self error: 'assignment to global not supported as a block arg'

%


set class RubyGlobalAsgnNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyGlobalAsgnNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyGlobalAsgnNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:gasgn, :', name, ', ', valueNode _inspect , $]

%


set class RubyGlobalAsgnNode
category: '*maglev-runtime'
method:
_irNodeSrcIr: srcIr
  | snd cref |
  (cref := RubyConstantRef new) globalVarName: name asSymbol .
  (snd := GsComSendNode new)
    rcvr: (GsComLiteralNode newObject: cref ) ;
    stSelector: #resolveGlobalVarAsgn:  ;
    appendArgument: ( self irValidateArg: srcIr ) .
  self ir: snd .
  ^ snd

%

