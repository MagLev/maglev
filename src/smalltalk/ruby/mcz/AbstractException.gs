
set class AbstractException class
category: '*maglev-runtime'
method:
addException: anException
  | res |
  (res := ExceptionSet new)
     addException: self ;
     addException: anException .
  ^ res

%


set class AbstractException
category: '*maglev-runtime'
method:
backtraceToLevel: aLevel 
 "convert the gsStack from AbstractException>>signal, if any
  to a Ruby stack report and cache it in gsStack."
| arr nativeStk |
arr := gsStack .
(arr _isArray and:[ (nativeStk := arr atOrNil:1 ) class == Boolean]) ifTrue:[
  "convert  from raw gsStack to Ruby backtrace Array "
  | backtrace level ofs arrSiz |
  backtrace := { } .
  level := 1 .
  ofs := 2 .
  arrSiz := arr size .
  [ level <= aLevel and:[ ofs < (arrSiz - 1) ]] whileTrue:[ | ip meth |
    level := level + 1.
    (ip := arr atOrNil: ofs + 1 ) ifNotNil:[ | env farr stepPoint |
      meth := arr at: ofs .
      ip < 0 ifTrue:[ ip := 0 ].
      nativeStk ifTrue:[ ip := meth _nativeIpOffsetToPortable: ip asReturn: false].
      stepPoint := meth _stepPointForIp: ip level: 2 isNative: nativeStk .
      farr := { 
          meth _descrForStack . 
          (meth _lineNumberForStep: stepPoint ) + meth _lineNumberBias.
         ( env := meth environmentId ) .
         meth homeMethod _rubyName 
      }.
      env ~~ 0 ifTrue:[ | fileLine |
          farr add: meth isRubyBridgeMethod .
          fileLine := meth _fileAndLine .
          fileLine ifNotNil:[ farr addAll: fileLine ].
      ].
      backtrace add: farr .
    ].
    ofs := ofs + 2 . 
  ].
  gsStack := backtrace .
  arr := backtrace . 
].
arr ifNil:[ arr := { } . gsStack := arr ].
^ arr

%

