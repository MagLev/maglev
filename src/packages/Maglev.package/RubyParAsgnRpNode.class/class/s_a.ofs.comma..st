*maglev-ast
s_a: lhs ofs: srcOfs comma: trailingCommaBool
  | res |
  (res := self _basicNew) firstNode: lhs; position: srcOfs ;
	trailingComma: trailingCommaBool .
  ^ res

