
doit
RubyAbstractVarNode subclass: 'RubyColon3Node'
	instVarNames: #( name globAssoc)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyColon3Node
removeallmethods
removeallclassmethods

set class RubyColon3Node class
category: 'Documentation'
method:
comment
 "MRI sexp stream uses   [:colon3, :Z ] for direct access to global  Z
  variable at outer most scope, such as    a=::Z   "

%


set class RubyColon3Node class
category: '*maglev-ast'
method:
s_a: nam_tok
  | res |
  (res := self _basicNew ) 
    name: nam_tok symval ;
    position: nam_tok src_offset .
  ^ res

%


set class RubyColon3Node
category: 'as yet unclassified'
method:
asClassNameNode
  | node |
  (node := RubyClassNameNode _basicNew)
     position: position ;
     name: name ; 
     isColon3: true .
  ^ node

%


set class RubyColon3Node
category: '(as yet unclassified)'
method:
baseName
  ^ name

%


set class RubyColon3Node
category: '(as yet unclassified)'
method:
definedQkind
  ( name at: 1) == $$ ifTrue:[ ^ #'global-variable' ].
   ^ #'constant' 

%


set class RubyColon3Node
category: 'as yet unclassified'
method:
determineDynamic
  ^ 0

%


set class RubyColon3Node
category: 'as yet unclassified'
method:
irClassNameLiteralNode
  | assoc |
  (assoc := globAssoc) ifNotNil:[ 
	  ^ assoc key irLiteralNode
  ].
  self error:'unhandled form of a class name'

%


set class RubyColon3Node
category: '*maglev-runtime'
method:
irDefinedQNode
  | assoc node  |
  self definedQkind == #'global-variable' ifTrue:[ ^ super irDefinedQNode ].

  (assoc := globAssoc) ifNil:[ self error:'colon3 irDefinedQNode missing assoc' ].
  node := assoc irDefinedQNode .
  self ir: node .
  ^ node

%


set class RubyColon3Node
category: '*maglev-runtime'
method:
irForDynamicCDecl: valIrNode
    |  node aNameNode  |   
    (aNameNode := RubySymbolNode newForIr) name: name .
    (node := GsComSendNode new) 
       rcvr:  (GsComLiteralNode newObject: Object  ) ;
       stSelector:   #rubyConstAt:put:   ;
       appendArgument:  aNameNode irNode ;
       appendArgument:  valIrNode .
    self ir: node .
    ^ node 

%


set class RubyColon3Node
category: '*maglev-runtime'
method:
irForModuleNesting
  ^ GsComLiteralNode newObject:  Module moduleNesting

%


set class RubyColon3Node
category: '(as yet unclassified)'
method:
irLeaf
  globAssoc ifNil:[ self error:'unresolved global during IR'].
  ^ self ir: (self irLitVarLeaf: globAssoc )

%


set class RubyColon3Node
category: '*maglev-runtime'
method:
irNode
  | callNode node assoc  |  
  (assoc := globAssoc) ifNotNil:[
    assoc class == RubyConstantRef ifFalse:[
      ^ super irNode   "an association bound at compile time, in bootstrap"
    ].
  ].
  "assoc is an instance of RubyConstantRef from walkWithScope"
  (node := GsComSendNode new)
     stSelector:  #resolveConst  ;
     rcvr: (GsComLiteralNode newConstantRef: assoc ) .
  self ir: node .
  ^ node

%


set class RubyColon3Node
category: '*maglev-runtime'
method:
isModuleClass
  ^ name == #Module

%


set class RubyColon3Node
category: 'as yet unclassified'
method:
leftIsDynamic
  ^ false

%


set class RubyColon3Node
category: 'accessing'
method:
name

	 ^ name

%


set class RubyColon3Node
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyColon3Node
category: '*maglev-runtime'
method:
pathArray
  ^ {  name asSymbol }

%


set class RubyColon3Node
category: 'as yet unclassified'
method:
walkForDefinedQ: aScope 
   ^ self walkWithScope: aScope isDefinedQ: true 

%


set class RubyColon3Node
category: 'as yet unclassified'
method:
walkWithScope: aScope
  ^ self walkWithScope: aScope isDefinedQ: false .

  "A  Colon3 node accesses a constant explicitly from top scope, like  ::Array"  
  "no lexical children"

%


set class RubyColon3Node
category: '*maglev-runtime'
method:
walkWithScope: aScope isDefinedQ: isQ
  "returns nil always"
  | rns assoc envId |
  aScope inBootstrap  ifTrue:[
    envId := RubyCompilerState current envId .
    rns := Object transientNameSpaceForStore: envId .
    assoc := rns resolveConstant: name . "constantLookup in root scope"
    assoc ifNotNil:[
      assoc _valueNoAction isNameSpace ifTrue:[  assoc := nil . "class not created yet"]
    ].
    assoc ifNil:[
       GsFile gciLogServer:'warning dynamic colon3 constant ', (RubyNameSpace pathToString: self pathArray)
    ].
  ].
  assoc ifNil:[ (assoc := RubyConstantRef _basicNew) c3name: name ].
  globAssoc := assoc .
  ^ nil

%


set class RubyColon3Node
category: 'as yet unclassified'
method:
walkWithScopeForCdecl: aScope
  "This usage is for a Colon3 node that is lhs of a RubyConstDeclNode.
   Returns the leftIsDynamic boolean "
     "no leftNode in a Colon3 node"
     "globAssoc left as nil, mRef in lexPath not needed since it is Object"
  ^  false 

%


set class RubyColon3Node
category: '*maglev-runtime'
method:
_inspect
  ^ '[:colon3, ', name , $]

%

