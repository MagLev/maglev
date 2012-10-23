*maglev-runtime
walkWithScope: aScope
  | itr |
  (itr := iterNode) ifNotNil:[
     "not in argNodes result, so walk here"
     itr walkWithScope: aScope . 
  ].
  RubyCompilerState current topMethodDef setHasBlockArgRef  .
  super walkWithScope: aScope 

