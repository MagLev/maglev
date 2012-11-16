*maglev-runtime
addImplicitBlockTemp

	hasImplicitBlockTemp ifNil: [
	  self addVariableNames: { self implicitBlockTempName }.
	  hasImplicitBlockTemp := true]

