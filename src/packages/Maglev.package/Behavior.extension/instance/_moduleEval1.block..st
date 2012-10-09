*maglev-runtime
_moduleEval1: lexPathNotUsed block: aBlock
    "A ruby primitive.  lexPath not used .
    Evaluate aBlock with the block's self and arg set to this module"
  | defStk envId cld |
  envId := 1"__callerEnvId" .
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: self .
  cld at: 7 put: self " _rubyThreadDataAt: 7 put: " .
  ^ [ | val |
     aBlock ifNotNil:[ | blk |
        blk := aBlock setSelf: self .
        val := blk @ruby1:value: self  
     ] .
     val
   ] ensure:[
     defStk pop: self
   ]

