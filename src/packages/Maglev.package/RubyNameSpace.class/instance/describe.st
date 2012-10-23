*maglev-runtime
describe
   "For debugging and topaz printing only"
  | path done scop nxt |
  path := { } .
  scop := nil .
  nxt := self .
  [ scop == nxt or: [nxt == nil] ] whileFalse:[
	 scop := nxt .
	 path insertAll: { scop name }  at: 1.
	 nxt := scop parent .
  ].  
  ^ '(RubyNameSpace ' , (self class pathToString: path) , ' lev ' , path size asString , ')'     

