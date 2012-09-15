
set class RubyService
category: 'examples'
classmethod:
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


set class RubyService
category: 'examples'
classmethod:
example2
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


set class RubyService
category: 'examples'
classmethod:
example3
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
	netSmtp start: 'localhost' do: [:smtp | smtp send_message: msg to_s _: fromAddress _: toAddress].
	^ msg to_s

%


set class RubyService
category: 'examples'
classmethod:
example4
	"Same as example3, but with reflective block sending"
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


set class RubyService
category: 'examples'
classmethod:
example5
	"Demo using implicit blocks"
	| ruby envKeys |
	ruby := RubyService new.
	
	envKeys := (ruby resolve: 'ENV') keys.
	^ envKeys map: [:element | element to_sym]

%


set class RubyService
category: 'examples'
classmethod:
example6
	"Demo using block syntax.
	 Compare to
	 	(1 to: 10) inject: 1 into: [:res :next | res / next]
	"
	| ary |
	ary := ((RubyService new resolve: 'Range') new: 1 _: 10).
	^ ary inject: 1 do: [:res :next | res / next]

%


set class RubyService
category: 'ruby-support'
method:
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


set class RubyService
category: 'ruby-support'
method:
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
           cld := GsProcess _current clientData .
           (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: (defCls := topSelf class) .
           cld at: 7 put: defCls " _rubyThreadDataAt: 7 put: " .
           res := topSelf performMethod: cm  .
        ]
      ] ifNotNil:[  "recursed to load another file"
        cld := GsProcess _current clientData .
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


set class RubyService
category: 'initialization'
method:
initialize
  envId := 1 .
  ctx := RubyContext load: #() env: envId .
  tns := Object transientNameSpaceForStore: envId .
  tns rubyGlobalVar: #'$0' put: 'Smalltalk Invocation' .

%


set class RubyService
category: 'ruby-support'
method:
loadGem: gemName
  "Ensure RubyGems is loaded, then load the ruby gem named gemName"
  self runRubySource: 'require "rubygems"; require "', gemName, '"' .

%


set class RubyService
category: 'ruby-support'
method:
require: requireName
  "Ensure RubyGems is loaded, then load the ruby gem named gemName"
  self runRubySource: 'require "', requireName, '"'

%


set class RubyService
category: 'ruby-support'
method:
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


set class RubyService
category: 'ruby-support'
method:
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


set class RubyService
category: 'ruby-support'
method:
runRubySource: source
  "Creates a fake file and then calls the RubyCompiler on it"
  |fakeFile|
  fakeFile := RubyEvalFile new.
  fakeFile fileName: 'Smalltalk Access to Ruby' .
  fakeFile source: source.
  ^ self runRuby: fakeFile

%

