
doit
RubyStaticScope subclass: 'RubyEvalScope'
	instVarNames: #( theBinding varLocs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyEvalScope
removeallmethods
removeallclassmethods

set class RubyEvalScope
category: '*maglev-runtime'
method:
binding: aBinding
  "instance initialization"
  theBinding := aBinding .
  varLocs := IdentityKeyValueDictionary new .

%


set class RubyEvalScope
category: '*maglev-runtime'
method:
includesTemp: aSymbol
  "called from C code in parser prim 903"
  | dict bnd |
  (varLocs at: aSymbol otherwise: nil) ifNotNil:[ 
     ^ true 
  ].
  (bnd := theBinding) ifNotNil:[ 
    ^ (bnd locationForExistingName: aSymbol) ~~ nil
  ].
  ^ false

%


set class RubyEvalScope
category: '*maglev-runtime'
method:
locationForExistingName: aSymbol depth: aNumber

  (aSymbol == #'$~' or:[ aSymbol == #'$_' or:[ aSymbol == #__lexPath ]]) ifFalse:[
    | loc ofs dict |
    (loc := varLocs at: aSymbol otherwise: nil ) ifNotNil:[ 
      ^ loc 
    ].
    theBinding ifNotNil:[ :bnd| 
      ofs := bnd locationForExistingName: aSymbol .
      ofs ifNotNil:[
        loc := RubyEvalVarLocation _basicNew .
        ofs _isSmallInteger ifTrue:[ loc vcOffset: ofs ] 
                           ifFalse:[ loc nameInBinding: aSymbol ].
        varLocs at: aSymbol put: loc . 
        ^ loc
      ].
    ].
  ].
  ^ nil   

%


set class RubyEvalScope
category: '*maglev-runtime'
method:
newVarLocation: aSymbol
  "returns a RubyEvalVarLocation"
  (aSymbol == #'$~' or:[ aSymbol == #'$_' or:[ aSymbol == #__lexPath ]]) ifFalse:[
    | loc |
    (varLocs at: aSymbol otherwise: nil) ifNotNil:[
      self error: 'duplicate newVarLocation'
    ].
    (loc := RubyEvalVarLocation _basicNew)  
       nameInBinding: aSymbol .
    varLocs at: aSymbol put: loc .
    ^ loc
  ].
  ^ nil

%

