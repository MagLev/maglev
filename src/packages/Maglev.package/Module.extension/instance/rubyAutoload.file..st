*maglev-runtime
rubyAutoload: aName file: fileToLoad
  "a ruby primitive.
   Register fileToLoad as the file to require when aSymbolName
   is referenced in the current name space."

  ^ (self transientNameSpaceForStore: 1"__callerEnvId") 
       rubyAutoload: aName asSymbol file: fileToLoad

