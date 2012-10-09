*maglev-runtime
irCompilerNode
   "sender responsible for installing source position in result"   
    ^ GsComSendNode new
        rcvr: ( GsComLiteralNode newObject:  RubyCompiler ) ;
        stSelector: #new 

