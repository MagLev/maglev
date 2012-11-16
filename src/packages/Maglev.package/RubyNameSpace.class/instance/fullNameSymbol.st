*maglev-cextensions
fullNameSymbol
  | par nm |
  ((par := parent) == nil or:[ par myClass == Object]) ifTrue:[ 
    nm := name .
    ^ nm ifNil:[ #'' ]
  ].
  nm := par fullName . 
  nm addAll: '::' ; addAll: name .
  ^ nm asSymbol .

