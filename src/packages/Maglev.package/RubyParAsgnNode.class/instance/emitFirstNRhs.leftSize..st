*maglev-runtime
emitFirstNRhs: count leftSize: lSize 
  | stms rList tmps lim  ass |
  stms := { } .    rList := rightList . tmps := evalTemps .
  lim := lSize >= count ifTrue:[ count -1  "masgn rhs same size or smaller" ] 
                       ifFalse:[ count "masgn rhs bigger" ].
  1 to: lim do:[:n | 
     (ass := GsComAssignmentNode _basicNew) dest: (tmps at: n) leaf  
                 source: (rList at: n ) irNode.
     stms add: ( self ir: ass ).
  ].
  lim == count ifFalse:[ "masgn rhs same or smaller,  Fix Trac629"
     (ass := GsComAssignmentNode _basicNew) dest: (tmps at: count) leaf  
               source: (rList at: count ) irNonSplatNode.
     stms add: ( self ir: ass ).
  ].
  ^ stms 

