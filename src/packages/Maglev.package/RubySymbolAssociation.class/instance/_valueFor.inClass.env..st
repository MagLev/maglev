*maglev-runtime
_valueFor: aSymbol inClass: aClass env: envId 
  
  isDefined ifNil:[ 
      ^  aClass @ruby1:const_missing: aSymbol 
  ].
  ^ value

