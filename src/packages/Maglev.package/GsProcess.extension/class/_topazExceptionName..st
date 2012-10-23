*maglev-runtime
_topazExceptionName: anException
  
 ^ [ | ecls |
      (ecls := anException class) _isExceptionClass ifFalse:[ ^ nil ].
      (SessionTemps current at:#RubyMainSelf otherwise: nil) ifNotNil:[
         "Ruby main program active, attempt Ruby execution to get name"
         [ ^ ecls @ruby1:name ] onException: AbstractException do:[:ex | ex return ].
       ].
       ecls name
    ] onException: AbstractException do:[:ex |
      ex return: '<error during _topazExceptionName:>'
    ].

