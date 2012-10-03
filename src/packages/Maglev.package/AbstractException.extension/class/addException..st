*maglev-runtime
addException: anException
  | res |
  (res := ExceptionSet new)
     addException: self ;
     addException: anException .
  ^ res

