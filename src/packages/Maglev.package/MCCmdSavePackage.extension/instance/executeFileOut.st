*maglev-override-execution
executeFileOut

	(self workingCopy package name = 'MagLev') ifFalse: [ ^ self ].
	(MCGsWriter new writeSnapshot: self workingCopy package snapshot) safeFileOut.

