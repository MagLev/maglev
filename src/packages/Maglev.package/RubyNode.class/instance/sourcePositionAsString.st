*maglev-runtime
sourcePositionAsString
  | pos |
  (pos := position) ifNil:[ ^ 'unknown source position'].
  pos _isInteger ifTrue:[ |  cst mth  | 
    cst := RubyCompilerState current .
   "( parser := cst topRubyParserOrNil) ifNotNil:[
      ^ 'line ' ,  ( parser with: pos perform: #line_for_offset: env:2 ) asString  
    ].
   "
   (mth := cst topMethodDefOrNil) ifNotNil:[  "use implementation in RubyRootNode"
     ^ [ 'line ' , (mth lineForOffset: pos) asString  
       ] on: ArgumentError do:[:ex |
         ^ 'byte ' , mth position asString
       ]
    ]
  ].
   ^ position asString  "SimpleSourcePosition from MRI parse server"

