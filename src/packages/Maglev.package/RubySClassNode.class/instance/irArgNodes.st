*maglev-runtime
irArgNodes
   | cst |
   cst := RubyCompilerState current .
   ^ { receiverNode irNode . 
         self literalNode . 
         cst topScope  implicitBlockVar .
         GsComLiteralNode newObject: 
            (Array withAll:(cst rtModuleStack ))
     } 

