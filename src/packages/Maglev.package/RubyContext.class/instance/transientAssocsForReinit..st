*maglev-runtime
transientAssocsForReinit: envId
  | arr tns |
  tns := Object transientNameSpaceForStore: envId .
  arr := { } .
  #( #STDIN  #'$stdin'  #STDOUT #'$stdout' #'$>' 
     #STDERR  #'$stderr'   #'$$'  #'$SAFE' 
   ) do:[ :sym |
     arr add: (tns associationAt: sym ) 
   ].
   ^ arr 

