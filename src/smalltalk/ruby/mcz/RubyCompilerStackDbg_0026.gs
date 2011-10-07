
set class RubyCompilerStackDbg
category: 'as yet unclassified'
method:
pop: anObj
  "RubyCompilerStackDbg  normally not used, change instance creation of a specific
   stack to use this class in order to use this debugging method."

  self size == 2 ifTrue:[(SessionTemps current at:#TrapPopEV otherwise:false) ifTrue:[ self pause ]].
  ^ super pop: anObj

%

