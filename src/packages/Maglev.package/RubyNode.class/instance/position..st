*maglev-runtime
position: pos
  pos _isSmallInteger ifFalse:[   "TODO remove check"
     nil pause .
     RubyParserM signalError: 'invalid source position'
  ].
  position := pos

