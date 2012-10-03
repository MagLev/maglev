*maglev-runtime
_forkvExec: commandStr
  "A ruby primitive.
   Result is an Array  { rawStatus . childStatus . resultString , errMsg, errno } "
| arr resultStr |
arr := System _performOnServer: commandStr .
resultStr := arr at: 3 .
resultStr ifNil:[  | rawStatus childStatus errno errMsg |
  rawStatus := arr at: 1 .
  childStatus := arr at: 2 .
  errno := arr at: 5 .
  errMsg := arr at: 4 .
  commandStr _error: #hostErrPerform args:{ errMsg . errno . rawStatus . childStatus }.
  ^ nil
].
GsProcess _current _rubyThreadDataAt: 2 "OC_RubyGsProcessClientData_childProcStatus+1"
            put:  (RubyProcessStatus with: arr ) .
^ arr

