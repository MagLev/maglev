
set class RubyClassNode class
category: '*maglev-runtime'
method:
s_a: aCpath b: aSuperNode c: aBody d: fileSourceString e: srcOfs
  | res |
  (res := self _basicNew )
     lineBias: 0  ;
     cpath: aCpath asClassNameNode ;
     superNode: aSuperNode ; position: srcOfs ;
     bodyNode: aBody source: fileSourceString .
  ^ res

%


set class RubyClassNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch, {  self superNode }

%


set class RubyClassNode
category: '*maglev-runtime'
method:
fixedInstVars: arrayNode
  "Returns true if ok, false if fixed instVars already installed"
  fixedIvs ifNil:[ fixedIvs := arrayNode . ^ true ].
  ^ false

%


set class RubyClassNode
category: '*maglev-runtime'
method:
irArgNodes
    | res n |
    res := super irArgNodes .
    (n := superNode) ifNotNil:[ res add: n irNode ].
    (n := fixedIvs) ifNotNil:[ res add: n irNode ]
                   ifNil:[ res add: ( GsComLiteralNode newObject: #())  ].
    ^ res

%


set class RubyClassNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ true

%


set class RubyClassNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		cr;
		nextPutAll: 'class ';
		printNode: cpath.
	superNode ifNotNil:
		[aStream
			nextPutAll: ' < ';
			printNode: superNode].
	aStream indentAndEnd: bodyNode

%


set class RubyClassNode
category: '*maglev-runtime'
method:
selector
   ^ superNode ifNil: [ #defineClassNamed:rubyMethod:inScope:fixedIvs: ]
               ifNotNil: [ #defineClassNamed:rubyMethod:inScope:superclass:fixedIvs:  ]

%


set class RubyClassNode
category: 'accessing'
method:
superNode

	 ^ superNode

%


set class RubyClassNode
category: 'accessing'
method:
superNode: aNode
	superNode := aNode

%


set class RubyClassNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | n |
  (n := superNode) ifNotNil: [ n walkWithScope: aScope ]. 
  super walkWithScope: aScope

%


set class RubyClassNode
category: '*maglev-runtime'
method:
_inspect
^  '
[:class,',  cpath _inspect, ', ', superNode _inspect, ', ', 
        bodyNode _inspect , ']
'

%

