*maglev-runtime
isInnerDef
   "Used by RubyColon2Node only .
    Return true if self is an inner def not counting defs above an eval"

   ^ outerDef ifNotNil:[:o | o ~~ self ] ifNil:[ false ]

