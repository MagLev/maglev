
doit
RubySymbolAssociation subclass: 'RubyClassVarAssociation'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyClassVarAssociation
removeallmethods
removeallclassmethods

set class RubyClassVarAssociation
category: '*maglev-runtime'
method:
definedQ

  isDefined ifNotNil:[ ^ 'class variable' copy ].
  ^ nil 

%


set class RubyClassVarAssociation
category: '*maglev-runtime'
method:
_freezeConstant
  "class variables may not be frozen"
  ^ self

%


set class RubyClassVarAssociation
category: 'as yet unclassified'
method:
_value
  isDefined ifNil:[ NameError signal:'uninitialized class variable ',key].
  ^ value

%


set class RubyClassVarAssociation
category: 'as yet unclassified'
method:
_valueOrNil
 
  ^ value

%

