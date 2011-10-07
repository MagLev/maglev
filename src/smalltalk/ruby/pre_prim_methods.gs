! methods that must be reloaded after each RubyContext reset
!   methods must be duplicated for each supported ruby environment
!  currently env 1 is customer's Ruby execution, 
!            env 2 is RubyParser execution only
!

! methods in environment 1 that should be private to smalltalk
!   need to have selector begin with '__'  to prevent them
!  from being visible to list-methods queries from ruby 

category: 'Ruby topaz support'

set compile_env: 1
method: Object
__topazPerform1: aSelector withArguments: argsArray
  "used by topaz SENDENV command."

^ self @env0:perform: aSelector env: 1 withArguments: argsArray
%

category: 'Ruby bootstrap support'
!---------------------------------
set compile_env: 0
method: Object
_singleton_method_added: aSymbol
  ^ self
%

set compile_env: 1
method: Object
__rubySend1: anArg
  " rubySend1... methods must be compiled with  'set compile_env: 1' so that
      __callerEnvId  will show env 1, in the method being performed , 
        after the with:...perform:env  frame  is reused for the method being performed "
    | sym |
    anArg _isSymbol ifTrue:[ 
       sym := anArg 
    ] ifFalse:[
       sym := (anArg _isArray ifTrue:[ (anArg @env0:at: 1) @ruby1:to_sym ]
                             ifFalse:[ anArg  ]) @ruby1:to_sym .
    ].
    sym := sym @env0:_asSymbolWithRubySuffix: 16r0 "#0__" .
    ^ self  @env0:perform: sym env: 1 .
%

set compile_env: 1
method: Object
__rubySend1: aSymbol with: anArg
  | sym |
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
      sym := aSymbol @ruby1:to_sym
  ].
  sym := sym @env0:_asSymbolWithRubySuffix: 16r4 . " #1__  "
  ^ self @env0:with: anArg perform: sym env: 1 .
%

set compile_env: 1
method: Object
__rubySend1: aSymbol with: anArg
  | sym |
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
      sym := aSymbol @ruby1:to_sym
  ].
  sym := sym @env0:_asSymbolWithRubySuffix: 16r4 . " #1__  "
  ^ self @env0:with: anArg perform: sym env: 1 .
%

set compile_env: 1
method: Object
__rubySend1: aSymbol with: argOne with: argTwo
   | sym |
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
    sym := aSymbol @ruby1:to_sym
   ].
   sym := sym @env0:_asSymbolWithRubySuffix: 16r8 . " #2__ "
  ^ self @env0:with: argOne with: argTwo perform: sym env: 1 
%

set compile_env: 1
method: Object
__rubySend1: aSymbol with: argOne with: argTwo with: argThree
   | sym |
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
    sym := aSymbol @ruby1:to_sym
   ].
   sym := sym @env0:_asSymbolWithRubySuffix: 16rC . " #3__ "
  ^ self @env0:with: argOne with: argTwo with: argThree perform: sym env: 1
%

set compile_env: 1
method: Object
__rubySend1: aSymbol withBlock: aBlock
  | sym |
  (sym := aSymbol) _isOneByteString ifFalse: [ "neither a String nor a Symbol"
    sym := aSymbol @ruby1:to_sym
  ].
   sym := sym @env0:_asSymbolWithRubySuffix: 16r1 . "  #0_& "
    ^ self @env0:withBlock: aBlock perform: sym env: 1 
%
set compile_env: 1
method: Object
__rubySend1: aSymbol withArgs: anArray block: blk 
   | sym  | 
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
      sym := aSymbol @ruby1:to_sym
   ].
   blk ifNil:[
     sym := sym @env0:_asSymbolWithRubySuffix: 16r2 . " #0*_  "
     ^ self @env0:withArgs:  anArray perform: sym env: 1 
   ] ifNotNil:[ 
     sym := sym @env0:_asSymbolWithRubySuffix: 16r3  .  " #0*&  "
     ^ self @env0:withArgs: anArray block: blk perform: sym env: 1 
   ]. 
%

set compile_env: 1
method: Object
__rubySend1: aSymbol withArgs: anArray 
   | sym  | 
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
      sym := aSymbol @ruby1:to_sym
   ].
   sym := sym @env0:_asSymbolWithRubySuffix: 16r2 . " #0*_ " 
  ^ self @env0:withArgs: anArray perform: sym env: 1 
%

set compile_env: 1
method: Object
__rubySend1: aSymbol with: argOne with: argTwo with: argThree block: blk 
   | sym |
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
    sym := aSymbol @ruby1:to_sym
   ].
   sym :=  sym @env0:_asSymbolWithRubySuffix: 16rD . " #3_& "
  ^ self @env0:with: argOne with: argTwo with: argThree with: blk perform: sym env: 1 
%

set compile_env: 1
method: Object
__rubySend1: aSymbol with: argOne with: argTwo block: blk 
   | sym |
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
    sym := aSymbol @ruby1:to_sym
   ].
   sym := sym @env0:_asSymbolWithRubySuffix: 16r9 . " #2_& "
  ^ self @env0:with: argOne with: argTwo with: blk perform: sym env: 1 
%

set compile_env: 1
method: Object
__rubySend1: aSymbol with: argOne block: blk 
   | sym |
   (sym := aSymbol) _isOneByteString ifFalse:[ "neither a String nor a Symbol"
    sym := aSymbol @ruby1:to_sym
   ].
   sym := sym @env0:_asSymbolWithRubySuffix: 16r5 . "  #1_& "
  ^ self @env0:with: argOne with: blk perform: sym env: 1 
%

set compile_env: 1
method: Object
__rubySend1: aSymbol block: blk 
      | sym |
    (sym := aSymbol) _isSymbol ifFalse:[
        sym := aSymbol @ruby1:to_sym
    ].
    sym := sym @env0:_asSymbolWithRubySuffix: 16r1 " #0_& " .
    ^ self @env0:with: blk perform: sym env: 1 
%

!---------------------------------
set compile_env: 0
method: Behavior
_method_added: aSymbol
  ^ self
% 

!---------------------------------
! reset compilation default to smalltalk env 0
set compile_env: 0
