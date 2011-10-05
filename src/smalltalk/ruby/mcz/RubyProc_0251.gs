
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

