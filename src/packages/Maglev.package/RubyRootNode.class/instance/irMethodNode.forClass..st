*maglev-runtime
irMethodNode: envId forClass: aClass
    | node cst num bias  |
    cst := RubyCompilerState current .
    envId == cst envId ifFalse:[ self error:'RubyRootNode>>irMethodNode: inconsistent envId'].
    (node := GsComMethNode newRuby) environment: envId ;
       setRubyLineNumberBias: (bias := lineNumberBias) ;
       lineNumber: 1 "to go with byte offsets from RubyParser" ;
       fileName: fileName source: source .
    cst pushMethodDef: self lineBias: bias .
    [
      staticScope buildTempsOn: node.
      self useScope: staticScope during: [
        isMainProgram == true ifTrue:[ self buildTopBindingOn: node ].
        bodyNode buildStatementsOn: node
      ].
      sendsBinding == true ifTrue:[ node forceAllArgsTmpsToVc  ].
      self ir: node.
    ] ensure:[
      cst popMethodDef: self lineBias: bias
    ].
    ^ node

