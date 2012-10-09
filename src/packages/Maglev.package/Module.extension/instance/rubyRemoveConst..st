*maglev-runtime
rubyRemoveConst: aSymbol
  "a ruby primitive.
   Remove the constant named aSymbol.  
   Pre-defined classes and singleton objects (e.g., true) can't be removed.
   TODO: Check if the constant is disallowed at a finer granularity.  The PickAxe book documents
   that you aren't allowed to remove the constants, but from irb, I can do: 
      Object.class_eval { remove_const :Object } 
   and it seems to work..."
  ^ self removeConst: aSymbol env: 1"__callerEnvId"

