*maglev-runtime
addException: anException
  | res |
  self == Object ifFalse:[ ArgumentTypeError signal:'expected anException or Object'].
  (res := ExceptionSet new)
     addException: Exception ;
     addException: anException .
  ^ res 

