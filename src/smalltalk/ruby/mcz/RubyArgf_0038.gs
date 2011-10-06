
set class RubyArgf
category: '*maglev-runtime'
classmethod:
with: anArgv
  ^ self _basicNew _init: anArgv

%


set class RubyArgf
category: '*maglev-runtime'
method:
_init: anArgv 
  (anArgv _isArray) ifFalse:[ ArgumentTypeError signal:'expected an Array'].
  argv := anArgv .
  stream := nil .
  lineno  := 0  .
  advance := true .
  closed := false .

%

