*maglev-runtime
_rubyInspect: envId
  "called from smalltalk code"
   self isMeta ifTrue:[  | res |
     res := '#<Class:' copy .
     res add: (destClass rubyFullName: envId  ) ;
         add:  $>  .
     ^ res
   ] ifFalse:[
     ^ self rubyFullName: envId 
   ]


