*maglev-runtime
argNodes
  "expect an ArgumentError at runtime, too few args,
   so don't bother to synthesize __FILE__ or __LINE__"
  ^ super argNodes

