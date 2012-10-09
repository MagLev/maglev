*maglev-runtime
atExit: aBlock
  aBlock _isExecBlock ifFalse:[ ArgumentTypeError signal:'argument must be a block'].
  RubyCompiler addExitHandler: aBlock .
  ^ aBlock

