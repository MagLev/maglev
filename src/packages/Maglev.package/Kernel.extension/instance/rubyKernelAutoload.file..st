*maglev-runtime
rubyKernelAutoload: aName file: aFile
  "a ruby primitive.
  Install an RubyAutoloadAssociation in the top level name space"
  ^ (Object nameSpace: 1"__callerEnvId" ) 
       rubyAutoload: aName asSymbol file: aFile 

