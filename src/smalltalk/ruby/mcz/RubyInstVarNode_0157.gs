
doit
RubyAbstractVarNode subclass: 'RubyInstVarNode'
	instVarNames: #( name stName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyInstVarNode
removeallmethods
removeallclassmethods

set class RubyInstVarNode class
category: '*maglev-ast'
method:
s_a: sym b: srcOfs

  | res |
  (res := self _basicNew ) name: sym ; position: srcOfs .
  ^ res

%


set class RubyInstVarNode
category: 'converting'
method:
accessKind

 ^ self instvarAccessKindFor: (stName ifNil:[ name ])

%


set class RubyInstVarNode
category: 'as yet unclassified'
method:
definedQNode
  | node |
  (node := RubyInstVarDefinedQNode newForIr)
     ivNode: self  .
  ^ node

%


set class RubyInstVarNode
category: 'as yet unclassified'
method:
determineDynamic
  " @ivname::X  signals TypeError at runtime"
  ^ nil

%


set class RubyInstVarNode
category: 'converting'
method:
irLeaf
	^ self compiler leafForInstVar: stName rubyName: name  with: self

%


set class RubyInstVarNode
category: '*maglev-runtime'
method:
irNode
 |  node  sym stSym |
 stSym := stName . 
 sym := name .
 RubyCompilerState current lexLevelOkForFixedIvAccess ifTrue:[
   (self instvarAccessKindFor: (stSym ifNil:[ sym ]) ) == 0 ifTrue:[
       ^ super irNode  "fixed or dynamic iv bytecode "
   ].
 ].
 "block in non-bootstrap code uses this path, since eval can change self of a block"
 stSym ifNil:[ stSym := (sym copyFrom: 2 to: sym size) asSymbol ].
 (node := GsComSendNode new)
    rcvr:  GsComVariableNode newSelf ;
    stSelector:  #_rubyInstvarAt:  ;
    appendArgument:  ( GsComLiteralNode newObject: { stSym . sym . nil . 0 } ).
 ^ self ir: node

%


set class RubyInstVarNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ self name = other name

%


set class RubyInstVarNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyInstVarNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyInstVarNode
category: 'converting'
method:
offset
	^ 1

%


set class RubyInstVarNode
category: 'as yet unclassified'
method:
pathArray
  " @ivname::X  signals TypeError at runtime"
  ^ nil

%


set class RubyInstVarNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: name

%


set class RubyInstVarNode
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


set class RubyInstVarNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:ivar, :', name , $]

%

