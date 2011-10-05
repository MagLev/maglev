
set class RubyLocalVarNode class
category: 'as yet unclassified'
method:
newForIr
  ^ self _basicNew

%


set class RubyLocalVarNode
category: 'as yet unclassified'
method:
definedQkind
  ^ #'local-variable'

%


set class RubyLocalVarNode
category: 'as yet unclassified'
method:
determineDynamic
   ^ 2  "use case    aLocalVar::ANAME "

%


set class RubyLocalVarNode
category: '*maglev-runtime'
method:
irBlockPassNode
      "ruby_selector_suffix dependent"
  "irBlockPassNode not sent if in bootstrap."
  useToProc > 0 ifTrue:[ "self is an incoming blockArg"
     ^ self irNode
  ] ifFalse:[ | send |
    ( send := GsComSendNode new)
       rcvr: self irNode ;
       rubySelector: #'__to_proc_arg#0__'  .
    self ir: send.
    ^ send 
  ]

%


set class RubyLocalVarNode
category: '*maglev-runtime'
method:
irNode 
      "ruby_selector_suffix dependent"
  | loc |
  loc := location .
  useToProc == 1 ifTrue:[ | lf tmpLf toprocSnd asgn tst |
    lf := loc leaf .
    tmpLf :=  loc varInfo toProcInfo leaf .
    (toprocSnd := GsComSendNode new)
       rcvr:( GsComVariableNode new leaf: lf );
       rubySelector: #'__to_proc#0__'  .
    asgn := GsComAssignmentNode _basicNew dest: tmpLf source: toprocSnd .
    (tst := GsComSendNode new)
       rcvr:( GsComVariableNode new leaf: tmpLf );
       stSelector: #ifNil: ;
       appendArgument:( self irInlineBlockIr: asgn ) ;
       optimize .
    self ir: toprocSnd ; ir: tst .
    ^ tst
  ] ifFalse:[  
    loc leaf ifNotNil:[ :lf |
      ^ self ir: (GsComVariableNode new leaf: lf)
    ].
    ^ loc irNode "fetch from var in an eval binding, trac 829"
  ]

%


set class RubyLocalVarNode
category: 'parsetree-as yet unclassified'
method:
isSameAs: other
	^ other location = self location
		and: [other name = self name]

%


set class RubyLocalVarNode
category: 'accessing'
method:
location

	 ^ location

%


set class RubyLocalVarNode
category: 'accessing'
method:
location: aNumber
	location := aNumber

%


set class RubyLocalVarNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyLocalVarNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyLocalVarNode
category: 'as yet unclassified'
method:
pathArray
   ^ { }   "use case    aLocalVar::ANAME "

%


set class RubyLocalVarNode
category: '*maglev-runtime'
method:
postWalkForYield
  | utp |
  useToProc ifNil:[ useToProc := 0 ]
         ifNotNil:[ useToProc := (utp := useToProc) + utp 
                    "use of incoming blk does not need to_proc" ].
  location varInfo readCount: -1  .

%


set class RubyLocalVarNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: self name

%


set class RubyLocalVarNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | loc |
  (loc := location) ifNil:[ 
      (loc := aScope locationForName: name) ifNil:[
          self error:'local var not found' .
      ].
      location := loc 
  ].
  useToProc := loc varInfo readCount: 1 .

%


set class RubyLocalVarNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:lvar, :', name , $]

%

