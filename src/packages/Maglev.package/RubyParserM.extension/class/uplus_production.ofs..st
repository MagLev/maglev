*maglev-ast
uplus_production: arg ofs: srcOfs
  | res |
  (arg isKindOf: RubyAbstractLiteralNode) ifTrue:[
     res := arg
  ] ifFalse:[
     res := self new_vcall: arg sel: (RpNameToken new: #'+@' position: srcOfs)
  ].
  ^ res

