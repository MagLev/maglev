*maglev-ast
new_defn: nameTok args: args body: bodyArg ofs: srcOfs 
     startLine: lineNum endOfs: endOfs
  | body result |
  bodyArg ifNil:[
    body := RubyBlockNode _basicNew 
  ] ifNotNil:[
    bodyArg class == RubyBlockNode ifTrue:[
      body := bodyArg
    ] ifFalse:[
      (body := RubyBlockNode _basicNew) list: {  bodyArg }
  ]].
  (result := RubyDefnNode s_a: nameTok symval b: args c: body )
    position:  srcOfs ; 
    startLine: lineNum endOffset: endOfs .
  ^ result

