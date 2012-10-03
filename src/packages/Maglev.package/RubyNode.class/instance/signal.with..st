(as yet unclassified)
signal: exceptionClass with: msg
  | fullMsg |
  (fullMsg := msg copy )
    add: ' , near ' ;
     add: self sourcePositionAsString .
  ^ exceptionClass signal: fullMsg

