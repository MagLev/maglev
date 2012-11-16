*maglev-runtime
_init: anArgv 
  (anArgv _isArray) ifFalse:[ ArgumentTypeError signal:'expected an Array'].
  argv := anArgv .
  stream := nil .
  lineno  := 0  .
  advance := true .
  closed := false .

