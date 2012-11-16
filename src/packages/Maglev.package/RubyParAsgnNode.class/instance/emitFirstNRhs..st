*maglev-runtime
emitFirstNRhs: count
  | stms rList tmps |
  stms := { } .    rList := rightList . tmps := evalTemps .
  1 to: count  do:[:n | | ass |
     (ass := GsComAssignmentNode _basicNew) dest: (tmps at: n) leaf  
				source: (rList at: n ) irNode.
     stms add: ( self ir: ass ).
  ].
  ^ stms 

