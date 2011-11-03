
set class Array
category: '*maglev-runtime'
method:
appendStatement: aNode
  self add: aNode

%


set class Array
category: '*maglev-runtime'
method:
_inspect
  | str sz |
  str := '[' copy .
  1 to: (sz := self size) do:[:j |
     str addAll: (self at: j) _inspect .
     j < sz ifTrue:[ str addAll: ', ' ].
  ].
  str addAll: ']' .
  ^ str

%


set class Array
category: '*maglev-runtime'
method:
_parAsgnCopyFrom: anInt
  | sz |
  anInt > (sz := self size) ifTrue:[
	^ { }
  ] ifFalse:[
    ^ self copyFrom: anInt to: sz 
  ]

%


set class Array
category: '*maglev-runtime'
method:
_rubySort: aSortBlock 
   "A ruby primitive.  Returns receiver.
    Sort receiver using aSortBlock. The block should take two arguments
    and return true if the first element should preceed the second one."

  aSortBlock ifNil:[ CannotReturn signal:'no block given'].
^ [ 
    self mergeSortFrom: 1 to: self size by: aSortBlock
  ] onException: RubyBreakException do: [:ex | | args |
    args := ex gsArguments .
    (args at: 1)  ifTrue:[  "Ruby break, terminate enumeration"
      ^ args at: 2
     ] ifFalse:[
        CannotReturn signal: 'Ruby retry during sort not supported'
     ]
   ] .

%

