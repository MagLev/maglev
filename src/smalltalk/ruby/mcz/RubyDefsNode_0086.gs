
set class RubyDefsNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubyDefsNode
category: 'accessing'
method:
argsNode: aNode
	argsNode := aNode

%


set class RubyDefsNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyDefsNode
category: 'accessing'
method:
bodyNode: aScopeNode
	bodyNode := aScopeNode

%


set class RubyDefsNode
category: '*maglev-runtime'
method:
defTargetAssignSelector
  ^ #theMetaClass:

%


set class RubyDefsNode
category: '*maglev-runtime'
method:
irTargetNode
   ^ receiverNode irNode

%


set class RubyDefsNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: 'def ';
		printNode: receiverNode;
		nextPutAll: '.';
		printNode: nameNode;
		parenthesize: argsNode;
		indentAndEnd: bodyNode

%


set class RubyDefsNode
category: 'accessing'
method:
receiverNode

	 ^ receiverNode

%


set class RubyDefsNode
category: 'accessing'
method:
receiverNode: aNode
	receiverNode := aNode

%


set class RubyDefsNode
category: 'converting'
method:
selector
	^ #compileSingletonFor:rubyMethod:

%


set class RubyDefsNode
category: 'as yet unclassified'
method:
walkWithScope: aScope
	"receiver belongs to current scope, the method defn gets a new scope"
   receiverNode walkWithScope: aScope .
   ^ super walkWithScope: aScope .

%


set class RubyDefsNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:defs, ', receiverNode _inspect, ', ', nameNode _inspect, ', ',
      argsNode _inspect, ', ', bodyNode _inspect , $]

%

