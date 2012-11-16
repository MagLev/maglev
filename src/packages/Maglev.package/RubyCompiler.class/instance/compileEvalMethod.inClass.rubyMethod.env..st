*maglev-runtime
compileEvalMethod: selectorArg inClass: aClass rubyMethod: aNode env: envId 
  ^ self compileMethod: selectorArg inClass: aClass rubyMethod: aNode env: envId 
		 	isEval: true

