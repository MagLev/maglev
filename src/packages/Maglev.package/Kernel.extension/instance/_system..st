*maglev-runtime
_system: commandStr
  "A ruby primitive.
   Result is an Array  { rawStatus . childStatus . childCompletedBool  . 
            errMsg. errno } "
| arr done |
arr := System __system: commandStr .
GsProcess _current _rubyThreadDataAt: 2 "GC_RubyGsProcessClientData_childProcStatus"
            put:  (RubyProcessStatus with: arr ) .
^ arr

