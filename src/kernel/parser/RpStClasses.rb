module FFI

  CByteArray = _resolve_smalltalk_global(:CByteArray)
end

module MagRp
  # resolution of all Smalltalk node classes 

  RubyNode = _resolve_smalltalk_global(:RubyNode)
#    RubyAbstractCallNode
#      RubyAbstractMatchDataRef
         RubyBackRefNode = _resolve_smalltalk_global(:RubyBackRefNode)
         RubyNthRefNode = _resolve_smalltalk_global(:RubyNthRefNode)
#      RubyAbstractMatchNode
         RubyMatch2Node = _resolve_smalltalk_global(:RubyMatch2Node)
         RubyMatchZeroNode = _resolve_smalltalk_global(:RubyMatchZeroNode)
       RubyAbstractWhileNode = _resolve_smalltalk_global(:RubyAbstractWhileNode)
         RubyUntilNode = _resolve_smalltalk_global(:RubyUntilNode)
         RubyWhileNode = _resolve_smalltalk_global(:RubyWhileNode)
       RubyAliasNode = _resolve_smalltalk_global(:RubyAliasNode)
       RubyGlobalVarAliasNode = _resolve_smalltalk_global(:RubyGlobalVarAliasNode)
       RubyAndNode = _resolve_smalltalk_global(:RubyAndNode)
#      RubyArgsCatNode
       RubyAttrAssignNode = _resolve_smalltalk_global(:RubyAttrAssignNode)
       RubyCallNode = _resolve_smalltalk_global(:RubyCallNode)
       RubyDotNode = _resolve_smalltalk_global(:RubyDotNode)
#      RubyDynamicDefinedQNode
       RubyEnsureNode = _resolve_smalltalk_global(:RubyEnsureNode)
       RubyFCallNode = _resolve_smalltalk_global(:RubyFCallNode)
#        RubyBlockGivenNode
#        RubyFCallBindingNode
#        RubyFCallEvalNode
       RubyGlobalLastExcBackTrace = _resolve_smalltalk_global(:RubyGlobalLastExcBackTrace)
       RubyGlobalNotAssignable = _resolve_smalltalk_global(:RubyGlobalNotAssignable)
       RubyHashNode = _resolve_smalltalk_global(:RubyHashNode)
       RubyIfNode = _resolve_smalltalk_global(:RubyIfNode)
#      RubyInstVarDefinedQNode
       RubyMethodDefNode = _resolve_smalltalk_global(:RubyMethodDefNode)
         RubyDefnNode = _resolve_smalltalk_global(:RubyDefnNode)
         RubyDefsNode = _resolve_smalltalk_global(:RubyDefsNode)
       RubyModuleNode = _resolve_smalltalk_global(:RubyModuleNode)
         RubyClassNode = _resolve_smalltalk_global(:RubyClassNode)
       RubyNotNode = _resolve_smalltalk_global(:RubyNotNode)
       RubyOpAsgnNode = _resolve_smalltalk_global(:RubyOpAsgnNode)
       RubyOpAsgnAndNode = _resolve_smalltalk_global(:RubyOpAsgnAndNode)
       RubyOpAsgnOrNode = _resolve_smalltalk_global(:RubyOpAsgnOrNode)
       RubyOpElementAsgnNode = _resolve_smalltalk_global(:RubyOpElementAsgnNode)
       RubyOrNode = _resolve_smalltalk_global(:RubyOrNode)
       RubyRescueNode = _resolve_smalltalk_global(:RubyRescueNode)
       RubySClassNode = _resolve_smalltalk_global(:RubySClassNode)
       RubySuperNode = _resolve_smalltalk_global(:RubySuperNode)
       RubyUndefNode = _resolve_smalltalk_global(:RubyUndefNode)
       RubyVCallNode = _resolve_smalltalk_global(:RubyVCallNode)
#        RubyVCallBindingNode
#        RubyVCallEvalNode
#        RubyVCallRaiseNode
       RubyYieldNode = _resolve_smalltalk_global(:RubyYieldNode)
       RubyZSuperNode = _resolve_smalltalk_global(:RubyZSuperNode)
#    RubyAbstractGotoNode
       RubyAbstractBreakNode = _resolve_smalltalk_global(:RubyAbstractBreakNode)
         RubyBreakNode = _resolve_smalltalk_global(:RubyBreakNode)
         RubyNextNode = _resolve_smalltalk_global(:RubyNextNode)
       RubyRedoNode = _resolve_smalltalk_global(:RubyRedoNode)
       RubyRetryNode = _resolve_smalltalk_global(:RubyRetryNode)
     RubyAbstractLiteralNode = _resolve_smalltalk_global(:RubyAbstractLiteralNode)
       RubyAbstractNumberNode = _resolve_smalltalk_global(:RubyAbstractNumberNode)
#        RubyBignumNode = _resolve_smalltalk_global(:RubyBignumNode)
         RubyFixnumNode = _resolve_smalltalk_global(:RubyFixnumNode)
         RubyFloatNode = _resolve_smalltalk_global(:RubyFloatNode)
       RubyFalseNode = _resolve_smalltalk_global(:RubyFalseNode)
       RubyNilNode = _resolve_smalltalk_global(:RubyNilNode)
       RubyRegexpNode = _resolve_smalltalk_global(:RubyRegexpNode)
       RubyStrNode = _resolve_smalltalk_global(:RubyStrNode)
         RubyXStrNode = _resolve_smalltalk_global(:RubyXStrNode)
       RubySymbolNode = _resolve_smalltalk_global(:RubySymbolNode)
       RubyTrueNode = _resolve_smalltalk_global(:RubyTrueNode)
#    RubyAbstractVarNode
       RubyClassVarNode = _resolve_smalltalk_global(:RubyClassVarNode)
       RubyColon3Node = _resolve_smalltalk_global(:RubyColon3Node)
         RubyColon2Node = _resolve_smalltalk_global(:RubyColon2Node)
           RubyClassNameNode = _resolve_smalltalk_global(:RubyClassNameNode)
       RubyConstNode = _resolve_smalltalk_global(:RubyConstNode)
#      RubyDVarNode
       RubyGlobalVarNode = _resolve_smalltalk_global(:RubyGlobalVarNode)
#        RubyGlobalLastException
#        RubyGlobalProcessNumber
       RubyInstVarNode = _resolve_smalltalk_global(:RubyInstVarNode)
       RubyLocalVarNode = _resolve_smalltalk_global(:RubyLocalVarNode)
#        RubyVcGlobalNode
       RubySelfNode = _resolve_smalltalk_global(:RubySelfNode)
     RubyArgsNode = _resolve_smalltalk_global(:RubyArgsNode)
     RubyArgumentNode = _resolve_smalltalk_global(:RubyArgumentNode)
#    RubyAssignableNode
       RubyClassVarDeclNode = _resolve_smalltalk_global(:RubyClassVarDeclNode)
       RubyConstDeclNode = _resolve_smalltalk_global(:RubyConstDeclNode)
       RubyDAsgnNode = _resolve_smalltalk_global(:RubyDAsgnNode)
       RubyGlobalAsgnNode = _resolve_smalltalk_global(:RubyGlobalAsgnNode)
#        RubyGlobalAsgnStdinNode
#        RubyGlobalAsgnStdoutNode
       RubyGlobalLastExceptionAsgn = _resolve_smalltalk_global(:RubyGlobalLastExceptionAsgn)
       RubyInstAsgnNode = _resolve_smalltalk_global(:RubyInstAsgnNode)
       RubyLocalAsgnNode = _resolve_smalltalk_global(:RubyLocalAsgnNode)
       RubyVcGlobalAsgNode = _resolve_smalltalk_global(:RubyVcGlobalAsgNode)
#        RubyVcGlobalLastMatchAsgn
#    RubyBeginNode
     RubyBlockArgNode = _resolve_smalltalk_global(:RubyBlockArgNode)
     RubyBlockPassNode = _resolve_smalltalk_global(:RubyBlockPassNode)
     RubyCaseNode = _resolve_smalltalk_global(:RubyCaseNode)
     RubyDefinedNode = _resolve_smalltalk_global(:RubyDefinedNode)
     RubyEvStrNode = _resolve_smalltalk_global(:RubyEvStrNode)
     RubyFlipNode = _resolve_smalltalk_global(:RubyFlipNode)
#    RubyIterNode  # not created from RubyParser
     RubyIterRpNode = _resolve_smalltalk_global(:RubyIterRpNode)
       RubyForNode = _resolve_smalltalk_global(:RubyForNode)
     RubyListNode = _resolve_smalltalk_global(:RubyListNode)
#      RubyArgsPushNode
       RubyArrayNode = _resolve_smalltalk_global(:RubyArrayNode)
         RubyRpCallArgs = _resolve_smalltalk_global(:RubyRpCallArgs)
       RubyBlockNode = _resolve_smalltalk_global(:RubyBlockNode)
       RubyDRegexpNode = _resolve_smalltalk_global(:RubyDRegexpNode)
         RubyDRegexpOnceNode = _resolve_smalltalk_global(:RubyDRegexpOnceNode)
       RubyDStrNode = _resolve_smalltalk_global(:RubyDStrNode)
         RubyDXStrNode = _resolve_smalltalk_global(:RubyDXStrNode)
       RubyDSymbolNode = _resolve_smalltalk_global(:RubyDSymbolNode)
#      RubyScopeNode
#    RubyNewlineNode
#    RubyParAsgnNode # not directly generated from RubyParser
     RubyParAsgnRpNode = _resolve_smalltalk_global(:RubyParAsgnRpNode)
#      RubyParAsgnStarNode
#    RubyParseErrorNode
     RubyRescueBodyNode = _resolve_smalltalk_global(:RubyRescueBodyNode)
     RubyReturnNode = _resolve_smalltalk_global(:RubyReturnNode)
     RubyRootNode = _resolve_smalltalk_global(:RubyRootNode)
#      RubyClassBodyNode
#      RubyEvalRootNode
     RubySplatNode = _resolve_smalltalk_global(:RubySplatNode)
#    RubyStarNode
     RubySValueNode = _resolve_smalltalk_global(:RubySValueNode)
     RubyToAryNode = _resolve_smalltalk_global(:RubyToAryNode)
     RubyWhenNode = _resolve_smalltalk_global(:RubyWhenNode)
#    RubyZArrayNode

  RpNameToken = _resolve_smalltalk_global(:RpNameToken)

end
MagRp._freeze_constants
