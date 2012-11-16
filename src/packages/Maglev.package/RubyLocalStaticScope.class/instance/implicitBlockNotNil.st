*maglev-runtime
implicitBlockNotNil
  | leaf |
  (leaf := incBlockArgLeaf) ifNil:[ ^ nil ].
  ^ hasImplicitBlockTemp == true
	ifTrue: [GsComVariableNode new leaf: (self _findVariable: self implicitBlockTempName) leaf]
	ifFalse: [GsComVariableNode new leaf: leaf].

