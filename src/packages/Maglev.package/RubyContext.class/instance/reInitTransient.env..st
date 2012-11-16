*maglev-runtime
reInitTransient: anArray env: envId
  | tns |
  RubyCompilerState initialize: envId .
  tns := Object transientNameSpaceForStore: envId .
  1 to: anArray size do:[ :n |  
	  tns addTransientAssociation: (anArray at: n)
  ].

  self _initTransient: envId .

