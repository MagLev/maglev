*maglev-runtime
_inspect
  | sep |
  sep := ',
  '.
  ^ '
[:block, ' , (self _inspect_list: sep parent: self) , ']
'

