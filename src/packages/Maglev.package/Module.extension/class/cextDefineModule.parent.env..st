*maglev-runtime
cextDefineModule: aString parent: parentArg env: envId
^ RubyCompiler new defineModuleNamed: aString asSymbol
     rubyMethod: nil inScope: parentArg  env: 1 

