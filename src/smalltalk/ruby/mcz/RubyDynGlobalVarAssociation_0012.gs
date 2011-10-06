
set class RubyDynGlobalVarAssociation
category: 'as yet unclassified'
classmethod:
newWithKey: aSymbol with: aBlock
  | assoc |
  assoc := self newWithKey: aSymbol .
  assoc block: aBlock .
  assoc setReadOnly .
  ^ assoc

%


set class RubyDynGlobalVarAssociation
category: 'as yet unclassified'
method:
block: aBlock
  theBlock := aBlock

%


set class RubyDynGlobalVarAssociation
category: 'as yet unclassified'
method:
globalValueHolder
  NameError signal:'aliasing of ' , key , ' not supported' .
  ^ nil

%


set class RubyDynGlobalVarAssociation
category: 'as yet unclassified'
method:
globalVarValue
  ^ theBlock value

%


set class RubyDynGlobalVarAssociation
category: 'as yet unclassified'
method:
globalVarValue: aValue
  NameError signal:'changing value of ' , key , ' not supported' .

%


set class RubyDynGlobalVarAssociation
category: 'as yet unclassified'
method:
invariantGlobalVarValue: aValue
  NameError signal:'changing value of ' , key , ' not supported' .

%

