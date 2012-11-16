*maglev-runtime
callCC
	"GsProcess>>value: fails to restore the clientData, we keep it in the RubyContinuation"
	| callCCReturn args clientData rubyCC |
	args := Array new: block numArgs.
	rubyCC := RubyContinuation new clientData: GsProcess _current clientData; yourself.
	args size > 0 ifTrue: [args at: 1 put: rubyCC].
	callCCReturn := [:cc |
		rubyCC continuation: (cc clientData: nil; yourself).
		block valueWithArguments: args] callCC.
	(GsProcess _current clientData isNil and: [(clientData := rubyCC clientData) notNil])
		ifTrue: ["resuming, restore client data" GsProcess _current clientData: clientData].
	^ callCCReturn
