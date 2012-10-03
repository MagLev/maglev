*maglev-runtime
traceRubyMethodDicts: aBoolean

  "Set value of the class variable which controls tracing of changes to
   enviroment 1 method dicts.  tracing is via GsFile gciLogServer:   "

  aBoolean _validateClass: Boolean .
  Error signal:'TraceRubyMethodDicts disabled' .

