*maglev-runtime
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

