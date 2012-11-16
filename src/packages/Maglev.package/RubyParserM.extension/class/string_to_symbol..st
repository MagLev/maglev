*maglev-ast
string_to_symbol: str
  str _isOneByteString ifFalse:[
    self signalError: 'internal_error, expected value to be a String'.
  ].
  str size == 0 ifTrue:[
    self signalError: 'empty symbol literal' .
  ].
  (str indexOfByte: 0 startingAt: 1) ~~ 0 ifTrue:[  
     self signalError: 'symbol string may not contain `\0'' ' .
  ].
  ^  str asSymbol

