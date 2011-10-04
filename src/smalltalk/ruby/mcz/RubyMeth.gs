
doit
RubyUnboundMeth subclass: 'RubyMeth'
	instVarNames: #( obj)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyMeth
removeallmethods
removeallclassmethods

set class RubyMeth
category: 'as yet unclassified'
method:
object: anObject
  obj := anObject

%


set class RubyMeth
category: '*maglev-runtime'
method:
unbind
  "a ruby primitive"
  | m |
  (m := RubyUnboundMeth new) method: gsmeth env: 1"__callerEnvId" selPrefix: selPrefix;
     bridge: execBridge .
  ^ m

%

