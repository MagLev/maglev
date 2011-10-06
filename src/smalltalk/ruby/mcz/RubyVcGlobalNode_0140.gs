
set class RubyVcGlobalNode
category: 'as yet unclassified'
classmethod:
newForRp
  ^ self _basicNew

%


set class RubyVcGlobalNode
category: 'as yet unclassified'
method:
determineDynamic
  ^ nil  " $_::  expected to be illegal"

%


set class RubyVcGlobalNode
category: '*maglev-runtime'
method:
irLeaf
  ^ location leaf

%


set class RubyVcGlobalNode
category: 'as yet unclassified'
method:
name: aName
  name := aName  

%


set class RubyVcGlobalNode
category: 'as yet unclassified'
method:
pathArray
  ^ nil  " $_::  expected to be illegal"

%


set class RubyVcGlobalNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | mth scp |
  mth := RubyCompilerState current topMethodDef .  
  scp := mth scope .
  mthScope := scp .
  (location := scp locationForName: name ) ifNil:[
     self error:'VcGlobal not found' 
  ]

%

