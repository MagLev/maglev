*maglev-runtime
lexLevel
	| level s |
	level := 0.
	s := self.
	[s enclosingScope notNil] whileTrue:
		[level := level + 1.
		s := s enclosingScope].
	^ level

