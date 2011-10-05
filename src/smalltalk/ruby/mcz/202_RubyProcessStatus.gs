
doit
Object subclass: 'RubyProcessStatus'
	instVarNames: #( stat primStatus)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyProcessStatus
removeallmethods
removeallclassmethods

set class RubyProcessStatus class
category: '*maglev-runtime'
method:
with: rawResultArray
  ^ self _basicNew init: rawResultArray

%


set class RubyProcessStatus
category: '*maglev-runtime'
method:
init: rawResultArray
  stat := rawResultArray at: 1 . "rawStatus from the child"
  primStatus := rawResultArray .

%


set class RubyProcessStatus
category: 'as yet unclassified'
method:
status
 ^ stat

%

