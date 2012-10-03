*maglev-ast
new_regexp: arg options: optionsSi
  | have_once arg_cls node options |
  have_once := false .
  options := optionsSi .
  (options bitAnd: 16r2000) ~~ 0 ifTrue:[ 
     have_once := true .
     options := options bitAnd:(16r2000 bitInvert).
  ].
  arg_cls := arg class .
  arg_cls == RubyStrNode ifTrue:[ | node |
    "simple regexp, don't care about have_once because no substitutions"
    [ | str |
      str := arg strNodeValue .
      (node := RubyRegexpNode _basicNew) regexpLit: ( Regexp new: str options: options lang: nil).
    ] on: RegexpError do:[:ex | 
      self signalError:'Regexp error, ' , ex messageText  .
    ].
    ^ node
  ].
  arg ifNil:[
    ^ RubyRegexpNode _basicNew regexpLit: ( Regexp new: '' options: options lang: nil)
  ].
  node := (have_once ifTrue:[ RubyDRegexpOnceNode] ifFalse:[ RubyDRegexpNode]) _basicNew .
  arg_cls == RubyDStrNode ifTrue:[
    node list: arg list .
  ] ifFalse:[
    node list: { RubyStrNode _basicNew _value: '' . arg }
  ].
  node options: options .
  ^ node

