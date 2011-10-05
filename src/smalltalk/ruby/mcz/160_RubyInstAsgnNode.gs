
doit
RubyAssignableNode subclass: 'RubyInstAsgnNode'
	instVarNames: #( name stName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyInstAsgnNode
removeallmethods
removeallclassmethods

set class RubyInstAsgnNode
category: '*maglev-ast'
method:
as_accessor
  ^ RubyInstVarNode _basicNew name: name

%


set class RubyInstAsgnNode
category: '*maglev-runtime'
method:
buildBlockArgumentsOn: irBlock

  | aLeaf argLeaf n asgnNod |
  aLeaf := self irLeaf .
  n := irBlock numArgs + 1 .
  (argLeaf := GsComVarLeaf new)
       blockArg: aLeaf varName argNumber: n forBlock: irBlock.
  irBlock appendArg: argLeaf .
  asgnNod := (GsComAssignmentNode _basicNew
      dest: aLeaf
      source: ( GsComVariableNode new leaf: argLeaf ) ).
  irBlock appendStatement: asgnNod

%


set class RubyInstAsgnNode
category: 'as yet unclassified'
method:
buildIrLeafsInto: anArray
   anArray add:  self irLeaf 

%


set class RubyInstAsgnNode
category: '*maglev-runtime'
method:
irAssignmentNode: srcIrNode
  |  node privSend sym stSym |
 stSym := stName .
 sym := name .
 (RubyCompilerState current lexLevelOkForFixedIvAccess) ifTrue:[ 
   (self instvarAccessKindFor: (stSym ifNil:[ sym ])  ) == 0 ifTrue:[
     ^ super irAssignmentNode: srcIrNode  "fixed or dynamic iv bytecode "
   ].
 ].
 stSym ifNil:[ stSym := (sym copyFrom: 2 to: sym size) asSymbol ].
 (privSend := GsComSendNode new)
   rcvr:  GsComVariableNode newSelf ;
   stSelector:  #rubyPrivateSize .
 (node := GsComSendNode new)
     rcvr:  GsComVariableNode newSelf ;
     stSelector:  #_rubyInstvarAt:put:privateSize: ;
     appendArgument:  ( GsComLiteralNode newObject: { stSym . sym . nil . 0 } );
     appendArgument:  srcIrNode ;
     appendArgument:  (self ir: privSend ) .
 ^ self ir: node

%


set class RubyInstAsgnNode
category: 'converting'
method:
irLeaf
	^ self compiler leafForInstVar:  stName rubyName: name with: self

%


set class RubyInstAsgnNode
category: 'converting'
method:
irNode
 
 ^ self irAssignmentNode: valueNode irEvaluatedBlockNode

%


set class RubyInstAsgnNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ other name = self name

%


set class RubyInstAsgnNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyInstAsgnNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyInstAsgnNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: name;
		nextPutAll: ' = ';
		printNode: valueNode

%


set class RubyInstAsgnNode
category: '*maglev-runtime'
method:
setIsBlockArg
  "do nothing"
  ^ self

%


set class RubyInstAsgnNode
category: 'as yet unclassified'
method:
walkWithScope: aScope
  | nam |
  nam := name.
  ( nam at: 1) == $@ ifFalse:[ self error:'instVar name does not start with @ '].
  (nam at: 2 equals:'_st_') ifTrue:[
    stName := ( nam copyFrom: 6 to: nam size) asSymbol 
  ].
  ^ super walkWithScope: aScope

%


set class RubyInstAsgnNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:iasgn, :', name, valueNode _inspect , $]

%

