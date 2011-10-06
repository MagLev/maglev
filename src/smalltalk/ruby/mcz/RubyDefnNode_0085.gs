
set class RubyDefnNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubyDefnNode
category: 'accessing'
method:
argsNode: aNode
	argsNode := aNode

%


set class RubyDefnNode
category: 'converting'
method:
bodyNode
	^  bodyNode

%


set class RubyDefnNode
category: 'accessing'
method:
bodyNode: aScopeNode
	bodyNode := aScopeNode

%


set class RubyDefnNode
category: 'parsetree-as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch

%


set class RubyDefnNode
category: '*maglev-runtime'
method:
defTargetAssignSelector
  ^ #theClass:

%


set class RubyDefnNode
category: '*maglev-runtime'
method:
irTargetNode
   | node |
   outerDef ifNotNil:[ :o | "within an eval or an outer def"
     (node := GsComSendNode new)
        rcvr: ( GsComLiteralNode newObject: GsProcess ) ;
        stSelector: #currentMethDefTarget .
   ] ifNil:[
     (node := GsComSendNode new) 
       rcvr: ( GsComLiteralNode newObject: GsProcess ) ;
       stSelector:  #methodDefnTarget .
   ].
   self ir: node .
   ^ node

%


set class RubyDefnNode
category: 'parsetree'
method:
isSameAs: other
	^ true

%


set class RubyDefnNode
category: 'converting'
method:
name
	^ nameNode sourceString

%


set class RubyDefnNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: 'def ';
		printNode: nameNode;
		parenthesize: argsNode;
		indentAndEnd: bodyNode

%


set class RubyDefnNode
category: 'converting'
method:
selector
	^ #compileIn:rubyMethod:

%


set class RubyDefnNode
category: '(as yet unclassified)'
method:
setIsInnerDef: outer
  innerDefs := 2 .
  outerIsDefsNode := true . "even if outer is a DefnNode"

%


set class RubyDefnNode
category: '*maglev-runtime'
method:
_inspect
 ^ '
[:defn, ', nameNode _inspect, ', ', argsNode _inspect, ', ', bodyNode _inspect , $]


%

