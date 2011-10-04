
doit
RubyColon2Node subclass: 'RubyClassNameNode'
	instVarNames: #( isColon3)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyClassNameNode
removeallmethods
removeallclassmethods

set class RubyClassNameNode
category: '*maglev-runtime'
method:
compileTimeClass 
  | cls assoc  |
  (assoc := globAssoc) ifNotNil:[
    cls := assoc _valueNoAction .
  ].
  ^ cls  "maybe nil"

%


set class RubyClassNameNode
category: 'as yet unclassified'
method:
determineDynamic
  isColon3 == true ifTrue:[ 
	 ^ ( leftIsDynamic := 1 ) 
  ].
  ^ super determineDynamic

%


set class RubyClassNameNode
category: 'as yet unclassified'
method:
irForClassName
  | node |
  (node := RubySymbolNode newForIr) name: name .
  ^ node irNode

%


set class RubyClassNameNode
category: '*maglev-runtime'
method:
irForClassParent
  "if the node is of form A::B , result is IR for
   constant evaluation up to the last ::  ,
   else result is IR for self 
  "
  isColon3 == true ifTrue:[ ^ GsComLiteralNode newObject: Object  ]. 
  ^ leftNode ifNotNil:[ 
     dynamicTypeError ifTrue:[ GsComLiteralNode newNil  ]
                       ifFalse:[  leftNode irNode ].
  ] ifNil:[
    GsComLiteralNode newObject: RubyCompilerState current topRtModule
  ].

%


set class RubyClassNameNode
category: 'as yet unclassified'
method:
isColon3
  ^ isColon3 == true

%


set class RubyClassNameNode
category: 'as yet unclassified'
method:
isColon3: aBoolean
  isColon3 := aBoolean

%


set class RubyClassNameNode
category: 'as yet unclassified'
method:
walkWithScope: aScope
  
  aScope inBootstrap ifTrue:[
	  "walk left node and resolve name for use in compileTimeClass"
	  ^ super walkWithScope: aScope  
  ] ifFalse:[
     |  isDynamic |
     (isDynamic := self determineDynamic ~~ 0)  ifTrue:[  
	   leftNode walkWithScope: aScope . "left node is to a Class or Module"
     ].
     ^ isDynamic
  ].

%


set class RubyClassNameNode
category: 'as yet unclassified'
method:
warnDynamicConst: aKey 
  ^ false

%


set class RubyClassNameNode
category: '*maglev-runtime'
method:
_inspect
  ^ isColon3 == true ifTrue:[ '[:classname, ::', name , $] ]
                    ifFalse:[ '[:classname, ', leftNode _inspect, ', ', name , $] ]

%

