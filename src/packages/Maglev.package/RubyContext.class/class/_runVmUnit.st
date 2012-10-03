*maglev-runtime
_runVmUnit 
"Run the vmunit tests from a topaz -l , assumming RubyContext load already done.
 uses currently configured MRI parser host and port ."

  ^ self _runVmUnit: nil options: #() 

