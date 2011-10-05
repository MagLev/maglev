
set class RubyConstNode class
category: '(as yet unclassified)'
method:
comment
  "No instances of RubyConstNode are produced anymore .
    :const  in sexpr stream  now produces a RubyColon2Node "

%


set class RubyConstNode
category: '(as yet unclassified)'
method:
fullName
  ^ name

%


set class RubyConstNode
category: '(as yet unclassified)'
method:
irLeaf
  ^ self error: 'should not be here, sexpr  :const  now produces a RubyColon2Node'

%


set class RubyConstNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ other name = self name

%


set class RubyConstNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyConstNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyConstNode
category: '*maglev-ast'
method:
node_assign_set_rhs: rhs
  "caller responsible for become:"
  | ofs c2n |
  c2n := RubyColon2Node sym: name srcOffset: (ofs := position) .
  ^ (RubyConstDeclNode s_a: c2n b: rhs ) position: ofs .

%


set class RubyConstNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: name

%


set class RubyConstNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  self error:'should not be here, sexp  :const  now produces RubyColon2Node'
  "no lexical children"

%


set class RubyConstNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:const, ', name _inspect , $]

%

