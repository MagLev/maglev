
doit
Object subclass: 'RubyMethodDefTarget'
	instVarNames: #( theClass)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyMethodDefTarget
removeallmethods
removeallclassmethods

set class RubyMethodDefTarget
category: '*maglev-runtime'
method:
theClass
  ^ theClass

%


set class RubyMethodDefTarget
category: '*maglev-runtime'
method:
theClass: aClass
  theClass := aClass .
  GsProcess _current _rubyThreadDataAt: 7 put: aClass .

%


set class RubyMethodDefTarget
category: '*maglev-runtime'
method:
theMetaClass
  ^ theClass theMetaClass

%


set class RubyMethodDefTarget
category: '*maglev-runtime'
method:
theMetaClass: aClass
  | cls |
  theClass := (cls := aClass _classForRubyClassVar ) .
  GsProcess _current _rubyThreadDataAt: 7 put: cls .

%

