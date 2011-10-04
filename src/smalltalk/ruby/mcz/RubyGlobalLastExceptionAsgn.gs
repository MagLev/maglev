
doit
RubyAssignableNode subclass: 'RubyGlobalLastExceptionAsgn'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyGlobalLastExceptionAsgn
removeallmethods
removeallclassmethods

set class RubyGlobalLastExceptionAsgn class
category: 'as yet unclassified'
method:
newForRp
  ^ self _basicNew

%


set class RubyGlobalLastExceptionAsgn
category: 'as yet unclassified'
method:
buildBlockArgumentsOn: irNode
  "block_spec.rb  has comments that rubinius does not support this"
  self error: 'assignment to global not supported as a block arg'
	

%


set class RubyGlobalLastExceptionAsgn
category: 'as yet unclassified'
method:
irLeaf
  "return VarLeaf for destination of the assignment"
  ^ RubyCompilerState current lastExceptionStack topOrNil

%


set class RubyGlobalLastExceptionAsgn
category: '*maglev-runtime'
method:
irNode
  | leaf node  |
   (leaf := self irLeaf)  ifNotNil:[ 
      node := (GsComAssignmentNode _basicNew
                  dest: leaf 
                 source:  (self typeCheckNodeFor: valueNode irEvaluatedBlockNode)) .
  ] ifNil:[ | gbl |
     "no enclosing rescue , assign to global $! "
     (gbl := RubyGlobalAsgnNode newForIr)
        name: #'$!' ;  valueNode: valueNode .
      node := gbl irNode .
  ].
   self ir: node .  
   ^ node 

%


set class RubyGlobalLastExceptionAsgn
category: 'as yet unclassified'
method:
name: aName
  "do nothing"

%


set class RubyGlobalLastExceptionAsgn
category: '*maglev-runtime'
method:
typeCheckNodeFor: valueIrNode
      "ruby_selector_suffix dependent"
  | node |
  (node := GsComSendNode new)
     rcvr: ( GsComLiteralNode newObject: Exception ); "Object rubyGlobalsAt:#Exception"
      rubySelector:  #'_validate#1__'  ;
      appendArgument: valueIrNode .
  ^ self ir: node 

%


set class RubyGlobalLastExceptionAsgn
category: '*maglev-runtime'
method:
_inspect
  ^ '[:gasgnLastExc, ', valueNode _inspect , $]

%

