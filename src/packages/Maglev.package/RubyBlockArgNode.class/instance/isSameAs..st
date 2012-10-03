parsetree
isSameAs: other
	self flag: #doWeNeedCount.
	^ self name = other name

