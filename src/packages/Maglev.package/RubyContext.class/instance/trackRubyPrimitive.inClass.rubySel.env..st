*maglev-runtime
trackRubyPrimitive: stMeth inClass: aClass rubySel: rubySel env: envId  
  "add to the rubyPrimMethods dictionary for this envId ,
   for use during code profiling."

  | dict list |
  dict := rubyPrimMethods at: envId .
  list := dict at: stMeth otherwise: nil .
  list ifNil:[
	 dict at: stMeth put: { aClass . rubySel }.
  ] ifNotNil:[ 
    list add: aClass ; add: rubySel .    
  ].

