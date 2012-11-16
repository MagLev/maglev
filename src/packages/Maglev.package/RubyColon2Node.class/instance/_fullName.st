*maglev-runtime
_fullName
 "for debugging and error messages"
  | nm |
  nm := String new .
  leftNode ~~ nil ifTrue:[ 
    nm addAll: leftNode _fullName ; addAll: RubyNameSpace delim
  ].
  nm addAll: name . 
  ^ nm

