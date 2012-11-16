*maglev-cextensions
rubyFullNameSymbol: envId
  "called from Smalltalk code only"
  | ns nam |
  (ns := self nameSpace: envId ) ifNotNil:[ ^ ns fullNameSymbol ].
  (nam := name) ifNil:[ ^ #''  ].  "Ruby meta classes have no name"
  ^ nam asSymbol

