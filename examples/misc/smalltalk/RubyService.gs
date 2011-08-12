doit
Object subclass: 'RubyService'
	instVarNames: #( ctx envId tns)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

! Remove existing behavior from RubyService
doit
RubyService removeAllMethods.
RubyService class removeAllMethods.
%
! ------------------- Class methods for RubyService
category: 'as yet unclassified'
set compile_env: 0
classmethod: RubyService
example1
	"Demo creating a mail using the mailfactory gem"
	| ruby mf msg |
	ruby := RubyService new.
	ruby loadGem: 'mailfactory'.

	mf  := ruby resolve: 'MailFactory'.

	msg := mf new.
	msg to: 'fred@here.com'.
	"ruby rubyPerform: 'to=' withArguments: #('fred@here.com') on: msg."
	"ruby rubyPerform: #'html=' withArguments: #('<h1>Hi there</h1>') on: msg."
	msg html: 'fred@here.com'.
	^ msg to_s
%
category: 'as yet unclassified'
set compile_env: 0
classmethod: RubyService
example3
	"Demo using the C-ext rdiscount gem"
	| ruby rd markdown html |
	ruby := RubyService new.
	ruby loadGem: 'rdiscount'.

	rd := ruby resolve: 'RDiscount'.
	markdown := rd new: '## Hello World
                                       This is *awesome*'.
	html := markdown to_html.
	^ html
%
category: 'as yet unclassified'
set compile_env: 0
classmethod: RubyService
example5
	| ruby clown face white_bg |
	ruby := RubyService new.
	ruby loadGem: 'RMagick'.
	
	clown := (RubyService resolve: 'Magick::ImageList') new: 'clown.jpg'.
	face := clown crop: 50 _: 15 _: 150 _: 165.
	white_bg := (RubyService resolve: 'Magick::Image') new: clown columns _: clown rows.
	clown := white_bg composite: face _: 50 _: 15 _: (RubyService resolve: 'Magick::OverCompositeOp').
	clown write: 'crop.jpg'.
%
category: 'as yet unclassified'
set compile_env: 0
classmethod: RubyService
example8
	| ruby msg html fromAddress toAddress netSmtp |
	ruby := RubyService new.
	ruby
		require: 'rubygems';
		require: 'mailfactory';
		require: 'rdiscount';
		require: 'net/smtp'.

	msg := (ruby resolve: 'MailFactory') new.
	html := ((ruby resolve: 'RDiscount') new: '## MagLev
This message was rendered from Gemstone/S, using the Ruby C-extension RDiscount via *MagLev*.') to_html.
	msg
		to: (toAddress := 'fred@here.com');
		from: (fromAddress := 'user@localhost');
		html: html;
		add_attachment: ((RubyEnv _getenv: 'MAGLEV_HOME'), '/README.rdoc') _: 'text/plain'.
	
	netSmtp := (ruby resolve: 'Net::SMTP').
	netSmtp
		rubyPerform: #start
		withArguments: #('localhost' 25 nil nil nil)
		withBlock: [:smtp | smtp send_message: msg to_s _: fromAddress _: toAddress].
	^ msg to_s
%
category: 'as yet unclassified'
set compile_env: 0
classmethod: RubyService
example9
	"Demo using blocks"
	| ruby envKeys |
	ruby := RubyService new.
	
	envKeys := (ruby resolve: 'ENV') keys.
	^ envKeys
		rubyPerform: #map
		withArguments: #()
		withBlock: [:element | element to_sym]
%
! ------------------- Instance methods for RubyService
category: 'ruby-support'
set compile_env: 0
method: RubyService
astFor: fakeFile
  "This method calls the parser on the source code in fakeFile and returns the ast.
   It emulates RubyCompiler parseFileNamed:loadName:"
  | trace warn  tmps source|

  tmps := SessionTemps current .
  trace := tmps at: #MagRpDEBUG otherwise: 0 .
  warn := tmps at:#MAGLEV_parseWarn otherwise: false .
  trace ~~ 0 ifTrue:[ GsFile gciLogServer:'Mel parse ' , (fakeFile fullPath) ].

  "The following:
      ast := RubyParserM rpParseFile: fullPath  loadName: aName yTrace: trace - 1 warnings: warn
      ^ ast
   reads the source out of the file, then calls rpParseString...
   result is after walkWithScopes has been done"

   source := fakeFile source .
   source immediateInvariant .
   ^ RubyParserM rpParseString: source
                 path: (fakeFile fullPath)
                 loadName: (fakeFile fullPath)
                 yTrace: trace -1
                 warnings: warn
%
category: 'ruby-support'
set compile_env: 0
method: RubyService
compileFile: file
"The fullPath is the file to compile.  aName is the name passed to require or load.
   The parser uses aName to fill out __FILE__

This method emulates RubyCompiler>>compileFileNamed:loadName:env:
There is (essentially) only one line that changed:

   - ast := self parseFileNamed: fullPath loadName: aName .
   + ast := self astFor: file .

I think RubyCompiler>>compileFileNamed:loadName:env: should be refactored
to take an AST (regardless of how it came to be. A file is only one way
to get an AST).  Then All of this cut-n-paste code could go away, and be
replaced with:
    ast := RubyParserM astFor: aFile .   ""Method to be written""
    ^ RubyCompiler compileAst: ast .     ""Method to be written"""

  | prevSelf compiler installingPrims |
  installingPrims := false .
^ [ | ast  res cst compStack defStk defCls |
    cst := RubyCompilerState initialize: envId .
    prevSelf  := GsProcess initRubyMainThread: installingPrims env: envId .

    "ast := self parseFileNamed: fullPath loadName: aName . "
    ast := self astFor: file .

    prevSelf ifNil:[ ast setMainProgram ].
    compiler := RubyCompiler new .
    (compStack := cst compilerStack) push: compiler .
    [ | cm cld |
      cm := compiler compileEvalMethod: #__compileFile inClass: Object
            rubyMethod: ast  env: envId .
      prevSelf ifNil:[
         compiler class withRubyHandlers: envId main: true do: [
           | topSelf |
           topSelf := SessionTemps current at: #RubyMainSelf .
           cld := GsProcess _current _clientData .
           (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: (defCls := topSelf class) .
           cld at: 7 put: defCls " _rubyThreadDataAt: 7 put: " .
           res := topSelf performMethod: cm  .
        ]
      ] ifNotNil:[  "recursed to load another file"
        cld := GsProcess _current _clientData .
        (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: (defCls := prevSelf class) .
        cld at: 7 put: defCls " _rubyThreadDataAt: 7 put: " .
        res := prevSelf  performMethod:  cm  .
      ].
    ] ensure:[
       defStk ifNotNil:[ defStk pop: defCls ].
       compStack pop: compiler .
       prevSelf ifNil:[ SessionTemps current at: #RubyMainCompiler put: nil ].
    ].
    res
  ] onException: AbstractException do:[:ex |
    prevSelf ifNil:[
      [ | lf msg |
        lf := Character lf .
        (SessionTemps current at:#Maglev_ruby_debugFlag otherwise: false) ifTrue:[
           msg := 'error , ' , ex asString,  ',
             during ' , (file fullPath) .
         ] ifFalse:[ |level|
           "Only print stack trace if warning level set and > 1"
           msg := nil .
                         level := tns rubyGlobalVar: #'$-W' .
                         level ifNil: [ level := 0 ] .
                         (level > 1) ifTrue: [
                                 msg := ex perform: #inspect env: envId .
              msg add: lf .
             (ex perform: #backtrace env: envId ) do:[:line | msg add: line; add: lf ].
           ] .
        ].
        msg ifNotNil: [ GsFile gciLogClient: msg ].
      ] onException: AbstractException do:[:exb |
          exb return: 'error during , ' , (file fullPath)
      ].
    ].
    ex outer
  ].
%
category: 'initialization'
set compile_env: 0
method: RubyService
initialize
  envId := 1 .
  ctx := RubyContext load: #() env: envId .
  tns := Object transientNameSpaceForStore: envId .
  tns rubyGlobalVar: #'$0' put: 'Smalltalk Invocation' .
%
category: 'ruby-support'
set compile_env: 0
method: RubyService
loadGem: gemName
  "Ensure RubyGems is loaded, then load the ruby gem named gemName"
  self runRubySource: 'require "rubygems"; require "', gemName, '"' .
%
category: 'ruby-support'
set compile_env: 0
method: RubyService
require: requireName
  "Ensure RubyGems is loaded, then load the ruby gem named gemName"
  self runRubySource: 'require "', requireName, '"'
%
category: 'ruby-support'
set compile_env: 0
method: RubyService
resolve: path
  "Resolves and returns the object in the Ruby namespace named by path.
   A path is a double-colon separated string like 'Maglev::System'."
  |ns components|
  ns := Object .
  components := path subStrings: '::'.
  (components at: 1) = 'Object' ifTrue:[ components removeAtIndex: 1 ].
  ^ RubyWrapper on: (components inject: Object
               into: [:ns :comp| (ns rubyConstAt: (comp asSymbol) env: 1) value ])
%
category: 'ruby-support'
set compile_env: 0
method: RubyService
runRuby: file
  "
   This is the entry point for running ruby.  The Compiler manages a stack
   of files, so we wrap our source code into a RubyEvalFile object, which
   is the Compiler's API for getting source code to work on.

   This method emulates RubyFile>>loadIntoEnv: We assume we are not loading
   a c-extension.
  "
  | oldFile fileStk |

  oldFile := tns at: #'__FILE__' otherwise: nil .
  tns at: #'__FILE__' compilePut: (file fullPath).
  fileStk := (RubyCompilerState initialize: envId) fileStack .
  fileStk push: file .
  ^ [ self compileFile: file ]
    ensure: [ fileStk pop: file .
              tns  at: #'__FILE__'  compilePut: oldFile. ]
%
category: 'ruby-support'
set compile_env: 0
method: RubyService
runRubySource: source
  "Creates a fake file and then calls the RubyCompiler on it"
  |fakeFile|
  fakeFile := RubyEvalFile new.
  fakeFile fileName: 'Smalltalk Access to Ruby' .
  fakeFile source: source.
  ^ self runRuby: fakeFile
%
doit
RubyService category: 'MagLev-Tools'
%
