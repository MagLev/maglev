*maglev-ast
new_defs: rcvr name: nameTok args: args body: bodyArg ofs: srcOfs 
    startLine: lineNum endOfs: endOfs 
  | body result |
  bodyArg ifNil:[
    body := RubyBlockNode _basicNew
  ] ifNotNil:[
    bodyArg class == RubyBlockNode ifTrue:[
      body := bodyArg .
    ] ifFalse:[
      (body := RubyBlockNode _basicNew) list: { bodyArg }
  ]].
  (result := RubyDefsNode s_a: nameTok symval b: args c: body )
     receiverNode: rcvr ; position: nameTok src_offset ; 
     startLine: lineNum endOffset: endOfs .
  ^ result

