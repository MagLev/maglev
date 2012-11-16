*maglev-runtime
rubyEval1: lexPath block: aBlock
  "A ruby primitive, for instance_eval.  lexPath is ignored .
   Evaluate aBlock with the block's self set to this object"
  | stk defnTarget cld |
  cld := GsProcess _current clientData .
  stk := cld at: 3 . " _rubyThreadDataAt: 3 , meth def target stack"
  stk push: (defnTarget := self _singletonClass: 1 ).
  cld at: 7 put: defnTarget . "_rubyThreadDataAt: 7 put: "
  ^ [ | val |
      aBlock ifNotNil:[ | blk |
        blk := aBlock setSelf: self . "copies aBlock if needed"
        val := blk @ruby1:value: self  .   
      ] .
      val
    ] ensure:[
      stk pop: defnTarget
    ]

