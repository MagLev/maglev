as yet unclassified
irArgNodes
  | arr irMth cst |
  cst := RubyCompilerState current .
  cst installingPrims ifTrue:[ 
	  self error:'creating a Binding not allowed during bootstrap'
  ].
  arr := argsList collect:[ :ea | ea irNode ].
  ( irMth  := cst topMethodDef comIrMethNode) ifNotNil:[
	 "append the real or implicit block leaf, which is last declared arg"
	 arr add: (GsComVariableNode new leaf:( irMth  arguments last)) .
	 hasBlkArg := true 
  ].
  ^ arr 

