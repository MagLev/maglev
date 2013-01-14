*maglev-runtime
childScopeAt: aSymbol isDefine: definBool env: envId 
 
  ^ (self transientNameSpaceForStore:  envId ) 
       childScopeAt: aSymbol isDefine: definBool env: envId 
  
  

