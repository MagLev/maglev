*maglev-runtime
header
  "A ruby primitive.
   Returns an Array, { originalName . comment . mtime }
   from header of a gzip file, or signals an error.  mtime is a time_t as an Integer"
  forWrite ifTrue:[ errorClass signal:'stream not opened for reading'].
  header ifNil:[
    self _readAndDecompress: 1"__callerEnvId"  .
    header == false ifTrue:[ errorClass signal:'stream has no gzip header'].
  ].
  ^ header

