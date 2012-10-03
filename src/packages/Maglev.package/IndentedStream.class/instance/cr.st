as yet unclassified
cr
	(self contents isEmpty or: [self contents last = Character cr or: [self contents last = Character tab]]) ifFalse:
		[super cr.
		self tab: self level]

