*maglev-cextensions
defineCextMeth: selPrefixStr cFunc: addressInt nArgs: nArgs prot: protInt
  "Install the C function at address addressInt as instance method 
   in env 1 of receiver with ruby selector selPrefixStr .
   library is left as nil ( resolves to entire process)
  
   protInt ignored for now."
 
  | cf |
  cf := CCallout cextNamed: selPrefixStr cFunc: addressInt nArgs: nArgs .
  cf _compileCextCaller: selPrefixStr In: self
   

