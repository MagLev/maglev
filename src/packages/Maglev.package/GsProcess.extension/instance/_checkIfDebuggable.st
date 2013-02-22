*maglev-runtime
_checkIfDebuggable
  "Check to make sure the receiver is debuggable. Currently this
   means that it is in the debug, suspended or ready states.
   If it is not debuggable then raise an error.

   Overridden for maglev, because we want to be able to step in suspended
   Ruby threads"


  | status |
  status := self _statusString.
  (#('ready' 'debug' 'active' 'suspended') includes: status) ifFalse: [
    ImproperOperation new
      _number: 2376;
      reason: #rtErrGsProcessNotDebuggable;
      object: self;
      signal].

