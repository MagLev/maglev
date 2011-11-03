
set class RubyProc
category: '*maglev-runtime'
method:
callCC
	"GsProcess>>value: fails to restore the clientData, we keep it in the RubyContinuation"
	| callCCReturn args clientData rubyCC |
	args := Array new: block numArgs.
	rubyCC := RubyContinuation new clientData: GsProcess _current _clientData; yourself.
	args size > 0 ifTrue: [args at: 1 put: rubyCC].
	callCCReturn := [:cc |
		rubyCC continuation: (cc _clientData: nil; yourself).
		block valueWithArguments: args] callCC.
	(GsProcess _current _clientData isNil and: [(clientData := rubyCC clientData) notNil])
		ifTrue: ["resuming, restore client data" GsProcess _current _clientData: clientData].
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

