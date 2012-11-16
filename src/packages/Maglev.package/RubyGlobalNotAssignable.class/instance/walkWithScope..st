*maglev-ast
walkWithScope: aScope
    "Thread is mapped to GsProcess during ruby prims install" 
  (msg := RubyStrNode newForIr )
     _value: 'global ' , name , ' is a read-only variable' .
 
  ^ super walkWithScope: aScope

