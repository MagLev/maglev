
set class RubyProc
category: '*maglev-runtime'
method:
callCC
	| clientData rubyReturn callCCReturn args |
	args := Array new: block numArgs.
	clientData := GsProcess _current _clientData. "callCC resume fails to restore the clientData, keep it"
	callCCReturn := [:cc |
		args size > 0 ifTrue: [args at: 1 put: (RubyContinuation with: cc)].
		block valueWithArguments: args] callCC.
	(clientData notNil and: [GsProcess _current _clientData isNil])
		ifTrue: ["resuming, restore client data"
			GsProcess _current _clientData: clientData].
	^ callCCReturn
%


set class RubyProc
category: '*maglev-runtime'
method:
selfValue
  ^ block selfValue

%


set class RubyProc
category: '*maglev-runtime'
method:
setSelf: aValue
  ^ block setSelf: aValue

%


set class RubyProc
category: '*maglev-runtime'
method:
value
  ^ block value

%


set class RubyProc
category: '*maglev-runtime'
method:
_copyForRuby: opcode
  "returns a new instance of the receiver's block 
    opcode 0 : copy for lambda,
    opcode 2 : copy for non-lambda  proc
  "
  | res |
  res := block _copyForRuby: opcode newBlockMethsInto: nil .
  res immediateInvariant .
  ^ res

%

