*maglev-ast
add_block_arg: arg
   arg ifNil:[ ^ self "do nothing, opt_f_block_arg term is nil"].
   blockArgNode ifNotNil:[
     RubyParserM signalError: 'block arg already installed'
   ].
   arg class == RubyBlockArgNode ifTrue:[
     blockArgNode := arg
   ] ifFalse:[ "used to expect an RpNameToken, this path " 
     RubyParserM signalError: 'add_block_arg invalid argument'  
   ].
   ^ self

