
set class ArgumentError
category: '*maglev-runtime'
classmethod:
signalTooFewArgs

  ^ self signal: 'too few arguments'

%


set class ArgumentError
category: '*maglev-runtime'
classmethod:
signalTooManyArgs

  ^ self signal: 'too many arguments'

%

