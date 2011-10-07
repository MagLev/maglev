
set class RubyColon2Node
category: '(as yet unclassified)'
classmethod:
comment
  ^ 'Used to access a global or constant such as
       a = MA::Y
     has this MRI sexp:
      [:lasgn, :a, [:colon2, [:const, :MA], :Y]]
    The  [:colon2...]  produces a RubyColon2Node
    
    Colon2 nodes also used as name nodes in
      RubyClassNode , RubyModuleNode and RubyConstDeclNode
   '

%


set class RubyColon2Node
category: 'as yet unclassified'
classmethod:
newForIr

  ^ self _basicNew  "position left as nil"

%


set class RubyColon2Node
category: '*maglev-ast'
classmethod:
sym: sym srcOffset: ofs
  | res |
  (res := self _basicNew) 
     name: sym ; position: ofs .
  ^ res

%


set class RubyColon2Node
category: '*maglev-ast'
classmethod:
s_a: left b: nam_tok
  | res |
  (res := self _basicNew ) 
    leftNode: left  ;
    name: nam_tok symval ;
    position: nam_tok src_offset .
  ^ res

%


set class RubyColon2Node
category: 'as yet unclassified'
method:
asClassNameNode
  | node |
  (node := RubyClassNameNode _basicNew) 
     position: position ;
     name: name ;
     leftNode: leftNode  . 
  ^ node

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
buildConstRef 
 | ref  |
 leftNode ifNotNil:[
   ref  := leftNode buildConstRef .
   ref ifNil:[ "lhs not a Colon2 node" 
     ref := { leftNode irNode }
   ].
   ref  add: name 
 ] ifNil: [ | cst lp |
   cst := RubyCompilerState current .
   lp := inInnerDef ifTrue:[ cst outerDefLexPath ]
                   ifFalse:[ cst rtModuleLexPath  ].
   (ref := RubyConstantRef _basicNew )
          name: name c2lexPath: lp evalSelfCls: cst topLexicalSelf   .
 ] .
 ^ ref 

%


set class RubyColon2Node
category: 'as yet unclassified'
method:
childrenForMatch
	^ {leftNode}

%


set class RubyColon2Node
category: '(as yet unclassified)'
method:
determineDynamic
 "returns  nil, 0, 1, 2 
  representing  typeError, not dynamic, dynamic , not cachable .
        saves the result in leftIsDynamic and dynamicTypeError "
 | val  lft  |
 (lft :=leftNode) ifNotNil:[
	(val := lft determineDynamic)  ifNil:[
	  dynamicTypeError := true .
	  val := 1 .
	] ifNotNil:[
	  dynamicTypeError := false .
	  val < 2 ifTrue:[ val := 1 ].
	].
 ] ifNil:[ 
    val := 0 .
    dynamicTypeError := false .
 ] .
 leftIsDynamic := val .
 ^ val

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
globalAssoc 
  "used during bootstrap only"  
  ^ globAssoc

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
irC2Node: aSelectorPair
  " selector pair is  #( fixed selector, dynamic resolve selector ) "
  | node cRef   |
  cRef := self buildConstRef .  
  cRef class == Array ifTrue:[ | arr lhsNode ofs |
    arr := cRef .
    cRef := RubyConstantRef new "initialize sets lexPathSize to zero" .
    lhsNode := arr at: 1 .
    cRef addAll: arr excludingFirst: 1 . "all path terms after first :: "
    ( node := GsComSendNode new)
       stSelector: ( aSelectorPair at: 2) ;  "dyn_resolveInContext: or dyn_definedQinContext:"
       rcvr: (GsComLiteralNode newConstantRef: cRef ) ;
       appendArgument: lhsNode .  
  ] ifFalse:[ 
    dynamicTypeError ifTrue:[ cRef setDynamicTypeError ].
    (node := GsComSendNode new)
       stSelector:  ( aSelectorPair at: 1)  ;  " resolveConst or definedQconst "
       rcvr: (GsComLiteralNode newConstantRef: cRef ) .
  ].
  self ir: node .
  ^ node

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
irDefinedQNode
  ^ self irC2Node: #( #definedQconst #dyn_definedQinContext: )

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
irForDynamicCDecl: valIrNode
    |  node  lhs  | 
    dynamicTypeError ifNotNil:[ self error: 'Colon2.irForDynamicCDecl - unexpected dynamicTypeError' ].
    node := GsComSendNode new .
    (lhs := leftNode) ifNil:[  | cst topMod |
      cst := RubyCompilerState current .
      topMod := cst topRtModule .
      node
        rcvr: (topMod == Object ifTrue:[ GsComLiteralNode newObject: topMod ] 
                    ifFalse:[ GsComVariableNode newSelf ] ); 
        stSelector:   #rubyConstDecl:put: ;
        appendArgument:  (GsComLiteralNode newObject: name) "a Symbol" ;
        appendArgument:  valIrNode .  "no longer using lexPath"
    ] ifNotNil:[
      node 
        rcvr:    lhs  irNode "a Colon2Node" ;
        stSelector:  #rubyConstAt:put:  ;
        appendArgument:  (GsComLiteralNode newObject: name) "a Symbol" ;
        appendArgument:  valIrNode .
    ].
    self ir: node .
    ^ node 

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
irNode
  globAssoc ifNotNil:[ leftNode ifNil:[ ^ super irNode "defer to colon3"]] . 
  ^ self irC2Node: #( #resolveConst  #dyn_resolveInContext: )

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
isModuleClass
  ^ name == #Module and:[ leftNode == nil ]

%


set class RubyColon2Node
category: 'as yet unclassified'
method:
isProcClass
  ^ isProcClass == true

%


set class RubyColon2Node
category: 'as yet unclassified'
method:
isSameAs: other
	^ other name = self name

%


set class RubyColon2Node
category: 'as yet unclassified'
method:
isTypeClass
  ^ isTypeClass == true

%


set class RubyColon2Node
category: '(as yet unclassified)'
method:
leftIsDynamic  
  ^ leftIsDynamic

%


set class RubyColon2Node
category: 'accessing'
method:
leftNode

	 ^ leftNode

%


set class RubyColon2Node
category: 'accessing'
method:
leftNode: aNode
	leftNode := aNode

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
pathArray
 | arr |
 leftNode ~~ nil ifTrue:[
    (arr := leftNode pathArray) ifNil:[
      arr := { } .
    ] ifNotNil:[
      arr  addLast: name asSymbol
    ].
 ] ifFalse:[
    arr := { name asSymbol } .
 ] .
 ^ arr

%


set class RubyColon2Node
category: 'printing'
method:
printSourceOn: aStream
	leftNode ifNotNil:
		[aStream
			printNode: leftNode;
			nextPutAll: '::'].
	aStream nextPutAll: name

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
walkWithScope: aScope isDefinedQ: isQ
  "returns nil always"
  | inBoot cst  |
  inBoot := aScope inBootstrap .
  cst := RubyCompilerState current .
  inInnerDef := cst topMethodDefOrNil ifNotNil:[:md | md isInnerDef ] ifNil:[ false ].
  (self determineDynamic ~~ 0 ) ifTrue:[
     leftNode walkWithScope: aScope isDefinedQ: isQ .
     inBoot ifTrue:[ | leftAssoc ln |
        isQ ifTrue:[ self error:' defined?  not supported during bootstrap'].
        ((ln :=leftNode) isKindOf: RubyColon2Node) ifFalse:[
            self error:'variable on lhs of :: not supported during bootstrap'].
        leftAssoc := ln globalAssoc .
        (leftAssoc isKindOf: Association) ifTrue:[ 
          globAssoc :=  leftAssoc _valueNoAction 
               bootConstantLookup: name env: cst envId .
        ].
     ].
  ] ifFalse:[   "left node is nil"
    inBoot ifTrue:[ | assoc ns rtModuStk | 
      rtModuStk := cst rtModuleStack .
      assoc :=  (ns := aScope nameSpace) ifNotNil:[ 
                     ns bootConstantLookup: name env: cst envId ] .
      assoc ifNil:[ 
        self warnDynamicConst: name 
      ] ifNotNil: [  | val key |
        isProcClass :=  (val := assoc _valueNoAction) == RubyProc .
        isTypeClass :=  (key := assoc key)  == #Type .
        (inBoot and:[ val isNameSpace ]) ifTrue:[  
          self warnDynamicConst: key .
        ].
      ].
      globAssoc := assoc .
    ] ifFalse:[ 
      isProcClass := name == #Proc  "it may be Proc,   Trac 538"
    ]
  ].
  ^ nil

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
walkWithScopeForCdecl: aScope
  "This usage is for a Colon2 node that is lhs of a RubyConstDeclNode.
   returns the leftIsDynamic value "
   | isDyn lft |
   (lft := leftNode) ifNotNil:[
      lft walkWithScope: aScope .
      isDyn := 1 .
   ] ifNil:[ 
      isDyn := 0
   ].
   leftIsDynamic := 0 .
   ^ isDyn 

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
warnDynamicConst: aKey 
  RubyContext bootWarnings == true ifTrue:[
      GsFile gciLogServer:
           'warning dynamic constant ', aKey , ', ',  self sourcePositionAsShortString .
  ].
  ^ true 

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
_fullName
 "for debugging and error messages"
  | nm |
  nm := String new .
  leftNode ~~ nil ifTrue:[ 
    nm addAll: leftNode _fullName ; addAll: RubyNameSpace delim
  ].
  nm addAll: name . 
  ^ nm

%


set class RubyColon2Node
category: '*maglev-runtime'
method:
_inspect
  ^  '[:colon2, ', leftNode _inspect, ', ', name , $]

%

