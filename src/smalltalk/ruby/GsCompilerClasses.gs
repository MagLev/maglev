! file  image/ruby/GsCompilerClasses.gs

expectvalue %String
run
"ensure GsCompilerClasses is in symbolList."

(System myUserProfile resolveSymbol: #GsCompilerIRNode) ifNil:[
  System myUserProfile symbolList addLast: (Globals at:#GsCompilerClasses) .
  ^ 'added GsCompilerClasses to symbol list'
] ifNotNil:[
  ^ 'GsCompilerClasses already in symbol list'
]
%
! fresh session to ensure symbolList changes visible
commit
logout
login
