
doit
RubyAssignableNode subclass: 'RubyLocalAsgnNode'
	instVarNames: #( location name isBlockArg
	                  createsVar hasAmpersand)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyLocalAsgnNode
removeallmethods
removeallclassmethods

set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
astDefaultVal
  | val |
  (val := valueNode) class == RubyLocalAsgnNode ifTrue:[
    ^ val astDefaultVal  "recurse to find right-most value in ruby   a=b=c=5 "
  ].
  ^ val

%


set class RubyLocalAsgnNode
category: '*maglev-ast'
method:
as_accessor
  ^ RubyLocalVarNode _basicNew name: name

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
buildBlockArgumentsOn: irBlock
  | argLeaf loc |
  isBlockArg ifFalse:[ self error:'inconsistent isBlockArg in LocalAsgn'].
  (argLeaf := (loc := location) leaf) ifNotNil:[ 
    irBlock appendArg: argLeaf .
    hasAmpersand ifNotNil:[ irBlock setAmpersandArg ].        
  ] ifNil:[  | n asgnNod | "assignment to a for loop variable in an eval"
    n := irBlock numArgs + 1 .
    (argLeaf := GsComVarLeaf new)
       blockArg:  name  argNumber: n forBlock: irBlock.
    irBlock appendArg: argLeaf .
    asgnNod := loc irAssignmentNode: ( GsComVariableNode new leaf: argLeaf ) .
    irBlock appendStatement: asgnNod
  ]

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
buildIrLeafsInto: anArray
   anArray add:  location leaf "assume not toProc conversion/clearing"

%


set class RubyLocalAsgnNode
category: 'parsetree-as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
irAssignmentNode: srcIrNode
   | asgn loc locVinfo  dLeaf varRef | 
   isBlockArg ifTrue:[ self error:'irNode illegal for LocalAsgn as outgoing block arg'].
   dLeaf := (loc := location) leaf .
   dLeaf ifNotNil:[
     asgn := GsComAssignmentNode _basicNew dest: (dLeaf := (loc := location) leaf)
                        source:  srcIrNode .
     self ir: asgn .
     (locVinfo := loc varInfo) readCount > 0 ifTrue:[  | clr |
       "need to clear the tmp caching result of __to_proc"
       createsVar ifTrue:[ 
           self error:'RubyLocalAsgnNode:both createsVar and (toProc)readCount > 0'
       ].
       clr := GsComAssignmentNode _basicNew dest: locVinfo toProcInfo leaf 
                                        source: GsComLiteralNode newNil .     
        varRef := GsComVariableNode new leaf: dLeaf .  
       ^ GsComStatementsNode new list:{  asgn . clr . varRef  }. 
     ] ifFalse:[
       ^ asgn
     ]
   ] ifNil:[
     ^ loc irAssignmentNode: srcIrNode "store into var in eval binding, trac 829"
   ]

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
irNode  
  ^ self irAssignmentNode: valueNode irLocalAsgnValue

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
irNodeNonDefault: irRhs block: irBlock
  "For a=b=c=5   a := non-default RHS , b:=nil, c:= nil"
  | val |
  val := valueNode.
  val class == RubyLocalAsgnNode ifTrue:[  "b=c=nil"
    irBlock appendStatement: (val irNodeWithDefault: GsComLiteralNode newNil)
  ].
  irBlock appendStatement: (self irAssignmentNode: irRhs) . "a := non-default"
  ^ nil

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
irNodeWithDefault: irRhs
  "Use the specified irRhs instead of the right-most value in ruby   a=b=c=5"
  | val |
  val := valueNode.
  val class == RubyLocalAsgnNode ifTrue:[ 
     ^ self irAssignmentNode: (val irNodeWithDefault: irRhs)
  ].
  ^ self irAssignmentNode: irRhs 

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
irOptArgNode
   | irCmp irArgVal astDefault leafLoc |
   leafLoc := location leaf .
   (irCmp := GsComSendNode new)
        rcvr: (GsComVariableNode new leaf: leafLoc );
        stSelector: #'=='  ;
        appendArgument: (GsComLiteralNode new 
          leaf: (GsComLitLeaf new specialLiteral:  UndefinedObject noArgNil )) .

    astDefault := self astDefaultVal .
    (irArgVal := GsComSendNode new)
        rcvr: irCmp ;
        stSelector: #ifTrue:ifFalse: ;
        appendArgument: (self newInlineBlock: [:block | 
             "for an arg like a=b=c=5   all vars get the default"
	    block appendStatement:( self irNodeWithDefault: astDefault irLocalAsgnValue) .  
            block ]) ;
        appendArgument: (self newInlineBlock: [:block | 
              "for a=b=c=5  ; a gets non-default pass value ; b,c get nil "
             self irNodeNonDefault: ( GsComVariableNode new leaf: leafLoc) block: block.
             block]);
        optimize .    "to make the ifTrue: inline"
    ^ irArgVal

%


set class RubyLocalAsgnNode
category: 'parsetree'
method:
isSameAs: other
	^ self location = other location 
		and: [self name = other name]

%


set class RubyLocalAsgnNode
category: 'as yet unclassified'
method:
isSingleIterArg
  ^ true

%


set class RubyLocalAsgnNode
category: 'accessing'
method:
location

	 ^ location

%


set class RubyLocalAsgnNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyLocalAsgnNode
category: 'accessing'
method:
name: aString
	name := aString .
	isBlockArg := false .

%


set class RubyLocalAsgnNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: name;
		nextPutAll: ' = ';
		printNode: valueNode

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
setHasAmpersand
  hasAmpersand := true

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
setIsBlockArg
  "isBlockArg == true means self is any argument to a block"
      
  isBlockArg := true  

%


set class RubyLocalAsgnNode
category: 'converting'
method:
tempName
	^ name

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
walkOptionalArg: aScope
  location := aScope locationForName: name .
  createsVar := false .

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
walkOptionalArgRhs: aScope
  super walkWithScope: aScope

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | loc | 
  isBlockArg ifTrue:[ 
     loc := aScope locationForBlockArg: name . 
     createsVar := false .
   ] ifFalse:[  | nm |
     loc := aScope locationForExistingName: (nm := name) depth: 0 . 
     loc ifNil:[ 
       loc := aScope newVarLocation: nm . 
       createsVar := true .
     ] ifNotNil:[
       createsVar := false .
     ].
  ].
  location := loc .
  super walkWithScope: aScope

%


set class RubyLocalAsgnNode
category: '*maglev-runtime'
method:
_inspect
  | res |
  res := '[:lasgn, ' copy .
  hasAmpersand ifNotNil:[ res add: $& ].
  res add: ' :'; add: name .
  valueNode ifNotNil:[ res addAll: ', ', valueNode _inspect ].
  res add: $]  .
  ^ res

%

