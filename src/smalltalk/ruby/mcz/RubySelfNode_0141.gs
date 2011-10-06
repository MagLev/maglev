
set class RubySelfNode
category: 'as yet unclassified'
classmethod:
new
  ^ self _basicNew   "creation during walkScopes, leave position nil"

%


set class RubySelfNode
category: 'as yet unclassified'
method:
definedQkind
  ^ #'self'

%


set class RubySelfNode
category: 'as yet unclassified'
method:
determineDynamic
  "deleted ticket 107 fix, 
   self can be LHS of a dynamic colon2 node 22oct08 "
   
  ^ 2

%


set class RubySelfNode
category: 'converting'
method:
irLeaf
	^ self ir: (GsComVarLeaf new initializeSelf)

%


set class RubySelfNode
category: 'parsetree-test'
method:
isSameAs: other
	^ true

%


set class RubySelfNode
category: 'as yet unclassified'
method:
pathArray
  "deleted ticket 107 fix, 
   self can be LHS of a dynamic colon2 node 22oct08 "
   
  ^ { }

%


set class RubySelfNode
category: 'as yet unclassified'
method:
printSourceOn: aStream
	aStream nextPutAll: 'self'

%


set class RubySelfNode
category: '*maglev-runtime'
method:
_inspect
  ^ ':self'

%

