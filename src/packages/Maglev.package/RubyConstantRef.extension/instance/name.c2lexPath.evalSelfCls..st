*maglev-runtime
name: aSymbol c2lexPath: lexPath evalSelfCls:  selfCls 
  "used by RubyColon2Node   "
  | lpSize  cnt newSiz |
  cnt := 0 .
  lpSize := lexPath size . 
  lexPathSize := lpSize .  
  self size: (newSiz := lpSize + 2) ;
       at: newSiz     put: aSymbol ;
       at: lpSize + 1 put: selfCls .
  lpSize ~~ 0 ifTrue:[  
    self replaceFrom: 1 to: lpSize with: lexPath startingAt: 1 .
  ].
  "globalAssoc left as nil"

