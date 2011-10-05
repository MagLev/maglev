
set class RubyModuleNode class
category: '*maglev-runtime'
method:
s_a: aCpath b: aBody c: fileSourceString d: srcOfs
  | res |
  (res := self _basicNew )
     lineBias: 0  ;
     cpath: aCpath asClassNameNode ; position: srcOfs; 
     bodyNode: aBody source: fileSourceString .
  ^ res

%


set class RubyModuleNode class
category: '*maglev-runtime'
method:
s_forRp: aCpath body: aBody source: fileSourceString 
  | res |
  (res := self _basicNew )
     lineBias: 0  ;
     cpath: aCpath asClassNameNode ; 
     bodyNode: aBody source: fileSourceString .
  ^ res

%


set class RubyModuleNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyModuleNode
category: '*maglev-runtime'
method:
bodyNode: aNode source:  fileSourceStr
  | bdy |
  (bdy := RubyClassBodyNode _basicNew)   
     bodyNode: aNode ;
     source: fileSourceStr "nil if using MRI parse server" ;
     lineBias: lineBias ;
     position: position .
  self class == RubyClassNode ifTrue:[ bdy classNode: self ].
  bodyNode := bdy 

%


set class RubyModuleNode
category: '(as yet unclassified)'
method:
comIrMethNode
   ^ nil  "not a method definition"

%


set class RubyModuleNode
category: 'accessing'
method:
cpath

	 ^ cpath

%


set class RubyModuleNode
category: 'accessing'
method:
cpath: aColon3Node
	cpath := aColon3Node

%


set class RubyModuleNode
category: '*maglev-runtime'
method:
irArgNodes
   ^ {  cpath irForClassName  . 
        self literalNode . 
        cpath irForClassParent  
     } .  

%


set class RubyModuleNode
category: '*maglev-runtime'
method:
irMethodNode: envId forClass: aClass 
  | irMeth |
  irMeth := self buildIrMethodNode: [ :node |
       scope buildTempsOn: node. 
       self useScope: scope during: [bodyNode buildStatementsOn: node].
  ].
  irMeth sourceOffset: position .
  ^ irMeth

%


set class RubyModuleNode
category: 'as yet unclassified'
method:
irNode
   | n |
   n := super irNode .
   cpath ir: n .
   ^ n 

%


set class RubyModuleNode
category: 'converting'
method:
irReceiverNode
   
	^ cpath ir: (self irCompilerNode) "give it the position of the class name node"

%


set class RubyModuleNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyModuleNode
category: 'as yet unclassified'
method:
lineBias
  ^ lineBias

%


set class RubyModuleNode
category: 'as yet unclassified'
method:
lineBias: anInt 
   lineBias := anInt

%


set class RubyModuleNode
category: 'converting'
method:
literalNode
	^ self ir: (GsComLiteralNode new leaf: (self ir: (GsComLitLeaf new methodLiteral: bodyNode )))

%


set class RubyModuleNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		cr;
		nextPutAll: 'module ';
		printNode: cpath;
		indentAndEnd: bodyNode

%


set class RubyModuleNode
category: 'accessing'
method:
scope

	 ^ scope

%


set class RubyModuleNode
category: '*maglev-runtime'
method:
selector
    ^ #defineModuleNamed:rubyMethod:inScope:

%


set class RubyModuleNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | newScop cp bdy cst | 
  (cp := cpath) walkWithScope: aScope .
  cst := RubyCompilerState current .
  scope := (newScop := RubyLocalStaticScope new ). 
  newScop
    requiredArgs: 0;
    restArg: -1;
    setGlobalScopeFor:  cp  parent: aScope env: cst envId .  
  [ (bdy := bodyNode) ifNotNil: [ |  modu  mRef file  |
      ( file := cst fileStack topOrNil) ifNotNil:[ 
         fileName := file fullPath .  source :=  file source 
      ].
      bdy walkWithScope: newScop
    ].
  ] ensure:[
    newScop _nameSpace: nil "avoid commit of tns"
  ]

%


set class RubyModuleNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:module ,',  cpath _inspect, ', ',  bodyNode _inspect , $]

%

