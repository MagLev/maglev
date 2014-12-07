!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Object_ruby.gs 26324 2011-08-02 23:25:18Z otisa $
!
!  Ruby support methods for Object
!=========================================================================
set class Object

category: 'Ruby Class Membership'
method:
class
"return first non-virtual non-singleton class at or above receiver's class.

 The old method  Object>>_class is not implemented in extent0.ruby.dbf
"
<primitive: 730>
^ self _primitiveFailed: #class
%

! see also env 1 Class>>class in  Class_ruby.gs

method:
virtualClass
"return receiver's class which may be a singleton or virtual class"
<primitive: 610>
^ self _primitiveFailed: #virtualClass
%

run
" Object>>_class not supported in ruby image"
(Object includesSelector: #_class) ifTrue:[ Object removeSelector: #_class ].
true
%

method: 
_isRubyString
"(Optimized selector.)  Returns false if the receiver is a Symbol,
 else if the receiver is an instance of String or a subclass thereof returns true,
 else returns false."

^ self _isRubyString
%


category: 'Ruby compare'
method:
_rubyEqualQ: anObject
  ^ self == anObject
%

!-------------------------------
category: 'Ruby Message Handling'

method:
_doesNotUnderstand: aSymbol args: anArray envId: envId reason: dnuKind

"Send from within the VM.
 Generates an error reporting that the receiver cannot respond to a message.
 because no compiled method was found for aSymbol in method environment
 envId.  If envId is 1 (Ruby), then call _method_missing to hook into the Ruby handling.
 envId is a SmallInteger, 0 for Smalltalk , 1 for Ruby , 
 2..255 for future use by Smalltalk package managers .

 This implementation must be in the persistent method dictionary for Object, 
 since the method is preloaded during VM startup.  
 Reimplementations in a session methods dictionary will not be seen.
"
dnuKind ~~ 0 ifTrue:[ self _dnuError: dnuKind args: anArray reason: dnuKind ]. 
envId == 1  ifTrue: [ |  prefix |
  prefix := aSymbol rubySelectorPrefixSymbol .   "Fix Trac 913"
  "prefix == #'a_prefix' ifTrue:[ nil pause ].  uncomment for debugging Ruby DNU"
  prefix ~~ #method_missing ifTrue:[ | args blk |
    (aSymbol _rubyOrdAt: -1) == 16r26 "== $& " ifTrue:[
      args := anArray _rubyAt: 0 length: anArray size - 1 .
      blk := anArray _rubyAt: -1 .
    ].
   args == nil ifTrue: [ args := anArray ].
   (aSymbol _rubyOrdAt: -2) == 16r2A " == $* " ifTrue: [
      "splat last arg if from a splat send. GitHub #221"
      args := args allButLast, args last
    ].
   blk == nil ifTrue: [
    ^ self @ruby1:method_missing: prefix __STAR: args 
  ] ifFalse: [
    ^ self @ruby1:method_missing: prefix __STAR: args __BLOCK: blk 
  ]].
  ^ self doesNotUnderstand: aSymbol args: anArray envId: envId
] ifFalse: [
  "smalltalk env 0"
  ^ self doesNotUnderstand: { aSymbol . anArray }
].
%

method:
_doesNotUnderstand: aPrefix args: anArray block: aBlock envId: envId 
  "used by ruby send implementation"

envId ~~ 0  ifTrue: [ 
  aPrefix ~~ #method_missing ifTrue:[
    aBlock ifNil:[
      ^ self @ruby1:method_missing: aPrefix __STAR: anArray 
    ] ifNotNil:[ 
      ^ self @ruby1:method_missing: aPrefix __STAR: anArray __BLOCK: aBlock
    ].
    ^ self doesNotUnderstand: aPrefix args: { anArray . aBlock } envId: envId
  ].
] ifFalse: [
  "smalltalk env 0"
  ^ self doesNotUnderstand: { aPrefix . anArray . aBlock }
].
%


method:
perform: aSymbol env: envId

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: #() envId: envId reason: 0
%

method:
with: argOne perform: aSymbol env: envId 

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: { argOne }  envId: envId reason: 0
%

method: Object
withArgs: anArray perform: aSymbol env: envId

<primitive: 2014>
"Assume aSymbol is of form #'foo#0*_' "
| prefix |
prefix := aSymbol rubySelectorPrefixSymbol .
^ self _doesNotUnderstand: prefix args: anArray block: nil envId: envId
%

method: Object
withBlock: argOne perform: aSymbol env: envId

<primitive: 2014>
"Assume aSymbol is of form #'foo#0_&' "
| prefix |
prefix := aSymbol rubySelectorPrefixSymbol .
^ self _doesNotUnderstand: prefix args: {} block: argOne envId: envId
%

method: Object
withArgs: anArray block: aBlock perform: aSymbol env: envId

<primitive: 2014>
"Assume aSymbol is of form #'foo#0*&' "
| prefix |
prefix := aSymbol rubySelectorPrefixSymbol .
^ self _doesNotUnderstand: prefix args: anArray block: aBlock envId: envId
%

method:
with: argOne with: argTwo perform: aSymbol env: envId

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: { argOne . argTwo }  envId: envId reason: 0
%

method
with: argOne with: argTwo with: argThree perform: aSymbol env: envId

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: { argOne . argTwo . argThree }  envId: envId reason: 0
%

method
with: argOne with: argTwo with: argThree block: blk perform: aSymbol env: envId

<primitive: 2014>
^ self _doesNotUnderstand: aSymbol args: { argOne . argTwo . argThree . blk }  envId: envId reason: 0
%


!-------------------------------
category: 'Ruby enumeration'
set class Object
method:
_rubyEach1: aBlock
  "A ruby primitive,
   All Ruby definitions of each&  compiled as definition of each&
   All Ruby sends of each&   compiled as send of __each&
   Object.rb  has    primitive_nobridge_env '__each&', _rubyEach ...  "
   
  ^ [
      "@ruby1:__each__:  translated to env1 send of #'each#0_&' by smalltalk parser"
      self @ruby1:__each__: aBlock
        "Ruby next   is handled by Bc_RUBY_NEXT bytecode which does a
         return from aBlock, thus letting the #'each&'  method
         process the next element of the enumeration"
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1) ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry,  restart the enumeration"
      ]
    ] .
%

method:
__superEach: aBlock
  "A ruby primitive in env 1 only .
   source copied by code in RubyCompiler and compiled in a Ruby class ,
   using smalltalk parsing, and 
   when a super send of #'each&' found  in a Ruby method in that class."
  
  ^ [
      "@ruby1:__each__:  translated to env1 send of #'each#0_&' by smalltalk lexer,
        comgen.c  uses RUBY_SUPER bytecode for send to super with env >= 1. "
      super @ruby1:__each__: aBlock
        "Ruby next   is handled by Bc_RUBY_NEXT bytecode which does a
         return from aBlock, thus letting the #'each&'  method
         process the next element of the enumeration"
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1) ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry,  restart the enumeration"
      ]
    ] .
%


!-------------------------------
category: 'Ruby support'
method:
_ruby_eqlQ: anObject

^ self == anObject
%

method:
_rubyNilQ

"implements  nil?  "

^ self == nil
%

!  _rubyKind_Block_String_Range_Regexp_Array deleted
!  _rubyTo: , _rubyTo_:    implemented in .mcz

method:
newRubySubclass: aString  instancesPersistent: ipersistBool fixedIvs: ivList
  "this is sometimes called when a class X < ModuleX inherits from a module"
  ArgumentTypeError signal: 'wrong argument type ', self class rubyFullName, '. expected a class.'
%

category: 'Ruby private'
method:
_addRubySingletonClass: committedOk envId: envId

"Insert a new singleton class in the receiver's class hierarchy.
 Returns the new class, or a String giving reason for failure
"
<primitive: 682>
committedOk _validateClass: Boolean .
envId _validateClass: SmallInteger .
(envId < 1 or:[ envId > 255]) ifTrue:[ OutOfRange signal:'invalid envId'].
self _primitiveFailed: #_addRubySingletonClass:envId:
     args: { committedOk . envId }
%


method:
_rubyInstvarAt: descrArray
"Return the value of the specified instance variable of the receiver,
 or nil if there is no such instance variable.
 Called from generated code. 

 descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset }
 stSymbol is used to search for fixed instVars, rubySymbol used to access
 dynamic instVars .

 Sends of this method are generated by IR generation phase of .mcz code.
"
<primitive: 768>  "prim fails if  defined?(instVar) would be false"
^ nil 
%

method:
_rubyInstvarDefined: descrArray
"Return the value of the specified instance variable of the receiver,
 or false if there is no such instance variable.
 Called from generated code. 

 descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset }
 stSymbol is used to search for fixed instVars, rubySymbol used to access
 dynamic instVars .

 Sends of this method are generated by IR generation phase of .mcz code.
"
<primitive: 768>  "prim fails if  defined?(instVar) would be false"
^ false 
%

method:
_rubyInstvarAt: descrArray put: aValue privateSize: privateSize
"Stores aValue of the specified instance variable of the receiver,
 creating a dynamic instance variable if needed.
 Returns aValue.
 Called from generated code.

 descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset }
 stSymbol is used to search for fixed instVars, rubySymbol used to access
 dynamic instVars .

 Signals a NameError if instVar found is private to
 Smalltalk or if receiver is a special object.

 Sends of this method are generated by IR generation phase of .mcz code.
"
<primitive: 769>
descrArray _validateClass: Array .
self _primitiveFailed:  #_rubyInstvarAt:put:privateSize:
     args: { descrArray . aValue . privateSize }
%

! deletion

method:
rubyPrivateSize
^ 0  "inline Object instSize"
%
classmethod:
rubyPrivateInstSize
^ 0
%
method:
isMetaOrModule
 ^ false
%
method:
_storeRubyVcGlobal: varDescr

" varDescr is a SmallInteger containing two fields:
    varIdx := varDescr bitAnd: 16rF
    caller := varDescr bitShift: - 4
  varIdx == 0 , Ruby scope-local global  $~
            1 , Ruby scope-local global  $_
            2 , Ruby scope-local eval lexical path
  caller specifies how many frames up stack to go to fetch $~
  and must be >= 2 . Returns nil if the caller value
  exceeds the stack depth.

 Stores the receiver into 's  $~ or $_ of specified frame,
 if frame's home context contains a reference to $~ or $_ .
 Returns receiver if a store was done,  returns nil if
 no reference to the global was found in specified frame's home context
"
<primitive: 772>
self _primitiveFailed: #_storeRubyVcGlobal: args: { varDescr }
%
method:
_getRubyVcGlobal: varIdx

" varDescr is a SmallInteger containing two fields:
    varIdx := varDescr bitAnd: 16rF
    caller := varDescr bitShift: - 4
  varIdx == 0 , Ruby scope-local global  $~
            1 , Ruby scope-local global  $_
            2 , Ruby scope-local eval lexical path
  caller specifies how many frames up stack to go to fetch $~
  and must be >= 2 . Returns nil if the caller value
  exceeds the stack depth.

 Returns value of $~ or $_ from specified frame.
 Returns nil if specified global
   does not exist in specified frame's home context.
"
<primitive: 773>
self _primitiveFailed: #_getRubyVcGlobal: args: { varIdx }
%

!  rubySingletonMethods: ... moved to mcz
! rubyMethods:  deleted

method:
_rubyInstVarDefinedQ: varDescr

"Used to implement Ruby      defined? @ivname

 varDescr must be a zero-based SmallInteger offset of a named
 instVar , or a Symbol naming a  dynamic instVar  .
 Returns #expression if the instVar is defined, otherwise returns nil.

 For a named instVar, defined means a ruby bytecode has stored
 into the instVar, and is determined by iv value ~~ remoteNil .
"
<primitive: 781>
self _primitiveFailed: #_rubyInstVarDefinedQ: args: { varDescr }
%

!  prim 688 also in Smalltalk image but with different failure code
method:
_respondsTo: aSymbol private: includePrivateBoolean flags: flags

"Returns true if receiver understands specified selector, false otherwise.
 flags is a Smallinteger with bit masks 
    environmentId                  16rFF  
    ruby lookup semantics         16r100
    ruby receiver is self        16r1000 (for future use)
    cache successes in code_gen 16r10000 
    all other bits ignored
"
<primitive: 688>
| envId |
envId := 1"__callerEnvId" .
aSymbol _isSymbol ifFalse:[ | sym |
  aSymbol _isOneByteString ifTrue:[
    "avoid creating symbols unnecessarily"
    (sym := Symbol _existingWithAll: aSymbol) ifNil:[ ^ false ].
  ].
  sym := [ 
    aSymbol @ruby1:to_sym 
  ] onSynchronous: Exception do:[:ex | 
    ArgumentError signal: 'expected a Symbol or String'
  ].
  sym _isSymbol ifTrue:[
    ^ self _respondsTo: sym private: includePrivateBoolean flags: flags
  ].
].
self _primitiveFailed:#_respondsTo:private:flags: 
     args: { aSymbol . includePrivateBoolean . flags }
%

method:
__bindingContext: numFrames
"Returns nil 
     or  an Array , { aVariableContext . nil . aGsNMethod . ... . homeGsNMethod} 
 for the frame which is numFrames above sender's sender's frame. 
 The result contains the VariableContext for specified frame (if any), 
 and a list of methods active on
 stack from current block(if any) up to homeMethod(if found), 
 based on searching the stack for references to the sender's VariableContext.
 Last element of result is  nil  if homeMethod is not found.
"
<primitive: 788>  
numFrames _validateClass: SmallInteger .
self _primitiveFailed: #__bindingContext: args: { numFrames }
%

method:
_bindingContext: numFrames
"Returns nil
     or  an Array , { aVariableContext . methodDefTarget . aGsNMethod . ... . homeGsNMethod}
 for the frame which is numFrames above sender's sender's frame.
 The result contains the VariableContext for specified frame (if any),
 and a list of methods active on
 stack from current block(if any) up to homeMethod(if found),
 based on searching the stack for references to the sender's VariableContext.
 Last element of result is  nil  if homeMethod is not found."

| arr |
arr := self __bindingContext: numFrames + 1 .
arr ifNotNil:[
  arr at: 2 put: GsProcess currentMethDefTarget
].
^ arr
%

method:
_rubyBasicDup

"Returns a copy of the receiver which shares the receiver's instance
 variables, using the non-singleton class of the receiver.
"

<primitive: 808>
self _primitiveFailed: #_rubyBasicDup .
self _uncontinuableError
%

method:
_rubyInitializeFrom: anObject

"copy fixed and dynamic instVars from anObject into self, growing
 self if needed to hold the dynamic instVars."

<primitive: 698>
"primitive fails if receiver and arg byte format and number of bytes different"
self _primitiveFailed: #_rubyInitializeFrom: args: { anObject } .
self _uncontinuableError
%

method:
_atCachedIv: anOffset put: aValue

"Returns aValue. Receiver is expected to be a literal object that will
 be referenced by memory pointer from a loaded method.  Store aValue
 into specified instVar without marking receiverDirty.  Used for
 RubyDRegexpOnceLiteral and similar ruby literals"

<primitive: 813>
"receiver is large object, or anOffset is out of range of named instVars"
self _primitiveFailed: #_atCachedIv:put: args: { anOffset . aValue }
%

method:
_rubyKindOf: aClass
  "a ruby primitive, to handle sends of kind_of? , is_a? "
  ^ self _rubyKindOf: aClass env: 1"__callerEnvId" 
%

method:
_rubyKindOf: aClass env: envId
  <primitive: 825>
  envId _validateClass: SmallInteger .
  self _primitiveFailed: #_rubyKindOf:env: args: { aClass . envId }
%

method:
__threadRubyEnvId
  AbstractException signal:'__threadRubyEnvId: should not be used'

"PREVIOUS USE, when using env2 for ruby compiler written in ruby"
  "Return value last saved by __threadSaveCallerEnv or __threadRubyEnvId:"

"  ^ self __threadRubyEnvId"  "an optimized selector , not a recursive send"
%
method:
__threadRubyEnvId: envId

  AbstractException signal:'__threadRubyEnvId: should not be used'
"PREVIOUS USE, when using env2 for ruby compiler written in ruby"
  "Set the current GsProcess's rubyEnvId .
   Returns previous value of rubyEnvId ."
" <primitive: 836> 

  self _primitiveFailed: #__threadRubyEnvId: args: { envId }
"
%
method:
__threadSaveCallerEnv

  AbstractException signal:'__threadSaveCallerEnv should not be used'
"PREVIOUS USE, when using env2 for ruby compiler written in ruby"
  "save __callerEnvId in current GsProcess's rubyEnvId .
   Not yet reentrant .  
   Returns the new value of rubyEnvId."
" <primitive: 835> 

  self _primitiveFailed: #_threadSaveCallerEnv
"
%
method
__callerEnvId

  AbstractException signal:'__callerEnvId should not be used'

"PREVIOUS USE, when using env2 for ruby compiler written in ruby"
  "Returns envId of the caller of the method containing the send of __callerEnvId"

  "Since Maglev is no longer using env 2 for the ruby compiler, we
   have hard coded   1""__callerEnvId"" in the Smalltalk methods for Maglev,
   so that when the caller is a _returnToC frame (from C extension) we
   will still get correct env.  Alternatively, the callback from C extension
   into the VM would need to push a different kind of _returnToC frame that
   showed as enviroment 1 ..."

 " ^ self __callerEnvId"  "an optimized selector , not a recursive send"
%

method:
_rubyBasicDupNamedIvs
  "Returns a copy of the receiver with named instVars (both fixed and dynamic)
   copied from the receiver,  and without copying any varying instVars.

   Example if receiver is an instance of a subclass of Array, 
   the result will have varing size 0,
   and will replicate all Ruby named instVars of the receiver. 
  "
  <primitive: 873>
self _primitiveFailed: #_rubyBasicDupNamedIvs
%

method:
_rubyNext: kindInt with: anFP

"Implements Ruby  'next' and 'redo' keywords .
 kindInt is  0 for next, 1 for redo .
 The argument to 'next' is the receiver.
 anFP must be nil at entry, and the stack word
 corresponding to anFP is private to the primitive."

<primitive: 2007>
self _primitiveFailed: #_rubyNext:with: args: { kindInt . anFP }
%
