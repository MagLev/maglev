
set class RubyVcGlobalAsgNode class
category: 'as yet unclassified'
method:
newForRp
  ^ self _basicNew

%


set class RubyVcGlobalAsgNode
category: 'as yet unclassified'
method:
irAssignmentNode: srcVarNode
  self error:'irAssignmentNode not valid for VcGlobal'

%


set class RubyVcGlobalAsgNode
category: '*maglev-runtime'
method:
irLeaf
  ^ location leaf

%


set class RubyVcGlobalAsgNode
category: 'as yet unclassified'
method:
location
  ^ location

%


set class RubyVcGlobalAsgNode
category: 'as yet unclassified'
method:
name: aName
  name:= aName

%


set class RubyVcGlobalAsgNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | mth scp |
  mth := RubyCompilerState current topMethodDef .
  scp := mth scope .
  mthScope := scp .
  (location := scp locationForName: name ) ifNil:[
     self error:'VcGlobal not found' 
  ].
  super walkWithScope: aScope 

%


set class RubyVcGlobalAsgNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:globalAsg, :', name, ', ', valueNode _inspect , $]

%

