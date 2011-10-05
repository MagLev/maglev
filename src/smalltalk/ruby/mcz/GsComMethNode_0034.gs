
set class GsComMethNode
category: '*maglev-runtime'
method:
selector_forBridge: aSymbol env: envId
  (GsComSelectorLeaf reimplementationAllowed: aSymbol inEnv: envId) ifFalse:[
     GsFile gciLogServer:'WARNING, reimplementation of  ', aSymbol , ' as bridge method may have no effect' .
  ].
  selector := aSymbol 

%

