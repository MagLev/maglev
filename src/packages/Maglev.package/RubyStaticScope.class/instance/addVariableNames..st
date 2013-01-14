*maglev-runtime
addVariableNames: anArray
  1 to: anArray size do:[:n | 
	  self locationForName: (anArray at: n )
  ].

