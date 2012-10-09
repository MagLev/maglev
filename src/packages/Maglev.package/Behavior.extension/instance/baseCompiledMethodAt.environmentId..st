*maglev-runtime
baseCompiledMethodAt: aSelector environmentId: envId

"Returns the compiled method associated with the argument aSelector (a String),
 not searching in the PackagePolicy .  Returns nil if no method found."

^ self compiledMethodAt: aSelector environmentId: envId otherwise: nil usePackages: false .

