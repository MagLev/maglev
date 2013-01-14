*maglev-runtime
persistentRubySuperclass: envId
  | mds |
  (mds := methDicts) _isArray ifFalse:[
	  ^ superClass "use env0 super class" 
  ].

  ^ (mds atOrNil: (envId*4 + 3) ) ifNil:[ superClass "use env0 super class"  ]

