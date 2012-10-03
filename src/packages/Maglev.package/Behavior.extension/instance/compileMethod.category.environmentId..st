*maglev-compiling
compileMethod: source category: cat environmentId: anEnvironmentId

	^ self compileMethod: source
      dictionaries: System myUserProfile symbolList
      category: cat
      environmentId: anEnvironmentId

