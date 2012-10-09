*maglev-runtime
error: msg 
  | fullMsg |
  (fullMsg := msg copy )
    add: ' , near ' ;
     add: self sourcePositionAsString .
  ^ Error signal: fullMsg 

