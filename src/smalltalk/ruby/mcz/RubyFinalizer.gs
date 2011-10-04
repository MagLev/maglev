
doit
Object subclass: 'RubyFinalizer'
	instVarNames: #( obj procs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyFinalizer
removeallmethods
removeallclassmethods

set class RubyFinalizer class
category: 'as yet unclassified'
method:
new: anObject proc: aProc

"a ruby primitive"
1"__callerEnvId" == 1 ifFalse:[ ArgumentError signal:'finalization only implemented for env 1'].
anObject isCommitted ifTrue:[
  ArgumentError signal:'define_finalizer not allowed on committed object'
].
"kindOf checks on aProc done in Ruby caller."

^ self _basicNew obj: anObject proc: aProc 

%


set class RubyFinalizer
category: 'as yet unclassified'
method:
addProc: aProc
  "a Ruby primitive"
  | theProcs |
  1"__callerEnvId" == 1 ifFalse:[ ArgumentError signal:'finalization only implemented for env 1'].
  (theProcs := procs) ifNil:[ ArgumentError signal:'finalization has already executed'].
  theProcs add: aProc .
  

%


set class RubyFinalizer
category: '*maglev-runtime'
method:
mourn
  | theProcs  theObj |
  "envId assumed 1" 
  theProcs := procs .
  theObj := obj .
  "do this iteration in Smalltalk to catch all possible exceptions"
  1 to: theProcs size do:[ :n | | aProc |
      aProc := theProcs at: n .
      [ 
         self @ruby1:__finalize: aProc _: theObj .
      ] onException: AbstractException do:[:ex | "ignore" ].
  ].
  self @ruby1:__finalize_done 

%


set class RubyFinalizer
category: 'as yet unclassified'
method:
obj: anObject proc: aProc 
  "debugging only:  GsFile gciLogServer: 'RubyFinalizer self  ' , self asOop asString , 
                         ' obj ' , anObject asOop asString . "
  obj := anObject .
  procs := { aProc }.
  self beEphemeron: true . 
  ^ self .

%

