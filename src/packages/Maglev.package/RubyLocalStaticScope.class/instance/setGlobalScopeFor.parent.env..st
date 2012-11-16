*maglev-runtime
setGlobalScopeFor: nameNode parent: aLocalScope env: envId 
  "nameNode should be a ClassNameNode"
  | parentGscop ns baseName c2path |
  variableNames := { } . "inline clearVariableNames" 
  inBootstrap := aLocalScope inBootstrap .
  nameNode isColon3 ifTrue:[
     parentGscop := Object 
  ] ifFalse:[  
    parentGscop := aLocalScope nameSpace .
    c2path := nameNode pathArray . 
    c2path size > 1 ifTrue:[   ns := parentGscop scopeAtPath: c2path env: envId ].
  ].            
  ns ifNil:[
    baseName := nameNode  baseName asSymbol .   
    ns := parentGscop ifNotNil:[ parentGscop childScopeAt: baseName isDefine: true env: envId ] .
  ].
  nameSpace := ns 

