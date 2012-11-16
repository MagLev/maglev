*maglev-runtime
scopeAtPath: aPath  env: eid  
  "Search upwards from receiver for a scope for which aPath is a suffix
   of the scope's full path."
  | scop nam |
 
  (aPath at: 1) = RubyNameSpace delim ifTrue:[ "a path beginning with :: "
      ^ (Object transientNameSpaceForStore: eid) 
          scopeAtPath: (aPath copyFrom: 2 to: aPath size)  env: eid
  ].
  scop := self scopeForPrefix: aPath isDefine: true env: eid  .
  ^ scop childScopeAt: aPath last isDefine: true env: eid 

