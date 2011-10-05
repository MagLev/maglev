
set class ArgumentError class
category: '*maglev-runtime'
method:
signalTooFewArgs

  ^ self signal: 'too few arguments'

%


set class ArgumentError class
category: '*maglev-runtime'
method:
signalTooManyArgs

  ^ self signal: 'too many arguments'

%

