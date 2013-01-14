*maglev-runtime
scopeAt: aDepth
	|  scope |
	scope := self.
	aDepth timesRepeat: [scope := scope enclosingScope].
	^ scope  

