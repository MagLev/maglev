*maglev-runtime
irArgNodes
    |  bodyLeaf   | 
      "nameLeaf := GsComLitLeaf new symbolLiteral: ( self methodSelector) .
       name will be obtained during IR generation of the method itself.
      "
    bodyLeaf := GsComLitLeaf new methodLiteral: self.
    ^      {  self irTargetNode .
          GsComLiteralNode new leaf: bodyLeaf  }

