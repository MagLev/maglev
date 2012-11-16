*maglev-runtime
comment
^ ' kinds are 
    #incomingBlock -  incoming block argument (&block) to a method definition
			(instance of RubyScopeIncomingBlockVar)
    #blockArg -  declared argument to a Block , like { |aBlockArg |  ... }
    #normal   -  method or block temp or , other incoming arg to method definition
    #evaluationTemp - temp created by IR generation, not declared in Ruby source
  '

