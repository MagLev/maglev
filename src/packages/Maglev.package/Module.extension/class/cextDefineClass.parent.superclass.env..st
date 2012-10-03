*maglev-runtime
cextDefineClass: aString parent: parentArg superclass: superCls env: envId
  "implementation for rb_define_class_under() .   
   does not support fixed instvars yet"

^ RubyCompiler new defineClassNamed: aString asSymbol 
   rubyMethod: nil inScope: parentArg superclass: superCls env: envId 
   fixedIvs: #()

