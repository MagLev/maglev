# We define all of these globals up front so that we can open these classes later without accidentally creating new ones

#            Ruby class name , Gemstone class name
RUBY.global('Array', 'Array')
RUBY.global('Behavior', 'Behavior')
RUBY.global('Binding', 'RubyBinding')
RUBY.global('Boolean', 'Boolean')
RUBY.global('Class', 'RClass')
RUBY.global('StClass', 'Class')
RUBY.global('ConditionVariable', 'TransientSemaphore')
RUBY.global('Dir', 'RubyDirectory')
RUBY.global('Env', 'RubyEnv')
RUBY.global('FalseClass', 'FalseClass')
RUBY.global('File', 'GsFile')
#  File::Stat --> GsFileStat  is done within File.rb
RUBY.global('Fixnum', 'SmallInteger')
RUBY.global('Float', 'Float')
RUBY.global('Hash', 'RubyHash')
RUBY.global('IdentityHash', 'RubyIdentityHash')
RUBY.global('Integer', 'Integer')
RUBY.global('Bignum', 'LargeInteger')
RUBY.global('BigDecimal', 'RubyBigDecimal')
RUBY.global('IO', 'IO')
RUBY.global('IPSocket', 'IPSocket')
RUBY.global('Kernel', 'Kernel')    # for module Kernel
RUBY.global('MatchData', 'MatchData')
RUBY.global('Method', 'RubyMeth')  # RubyMeth , RubyUnboundMeth defined in .mcz
RUBY.global('Module', 'Module')    # for class Module
RUBY.global('Mutex', 'TransientMutex')
RUBY.global('NilClass', 'UndefinedObject')
RUBY.global('Numeric', 'Number')
RUBY.global('Object', 'Object')
RUBY.global('Proc', 'RubyProc')
RUBY.global('Random', 'TransientRandom')
RUBY.global('Range', 'Range')
RUBY.global('Regexp', 'Regexp')
RUBY.global('IdentitySet', 'IdentitySet')
RUBY.global('SmallDouble', 'SmallDouble')
RUBY.global('Socket', 'RubySocket')
RUBY.global('String', 'String')
RUBY.global('Symbol', 'Symbol')
RUBY.global('TCPServer', 'TCPServer')
RUBY.global('TCPSocket', 'TCPSocket')
RUBY.global('Thread', 'GsProcess')
RUBY.global('ThreadGroup', 'RubyThreadGroup')
RUBY.global('Time', 'RubyTime')
RUBY.global('TransientShortArray', 'TransientShortArray')
RUBY.global('TrueClass', 'TrueClass')
RUBY.global('UnboundMethod', 'RubyUnboundMeth')

RUBY.global('Gemstone', 'System')
RUBY.global('RubyContext', 'RubyContext')
RUBY.global('CZstream', 'CZstream')

#
#  Define some of the Ruby exception classes to be identical to certain
#   Smalltalk exception classes   .
#  see GlobalErrors.rb for additional creation of Ruby exception classes
#
#            Ruby name           Smalltalk Name
#
RUBY.global('Exception',    'UserException')
  RUBY.global('SystemExit',     'RubySystemExit')
  RUBY.global('SystemStackError',    'StackOverflow')
  RUBY.global('NoMemoryError',  'OutOfMemory')
  RUBY.global('ScriptError',      'RubyScriptError')
    RUBY.global('LoadError',        'RubyLoadError')
    RUBY.global('NotImplementedError',  'RubyNotImplementedError')
    RUBY.global('SyntaxError',  'RubyParseError')

  RUBY.global('SignalException',    'ControlInterrupt')

  RUBY.global('StandardError',    'Error')
    #IntepreterError is here in Smalltalk hierarchy
      RUBY.global('ArgumentError',  'ArgumentError')
      RUBY.global('IOError',        'IOError')
        RUBY.global('EOFError',     'EOFError')
        RUBY.global('SocketError',  'SocketError')
      RUBY.global('IndexError',     'OffsetError')
      RUBY.global('LocalJumpError',     'CannotReturn')
      RUBY.global('RangeError',     'RangeError')
        RUBY.global('FloatDomainError', 'FloatingPointError')
      RUBY.global('RegexpError',    'RegexpError')
      RUBY.global('RuntimeError',   'RuntimeError')
      RUBY.global('SecurityError',  'SecurityError')
      RUBY.global('SystemCallError',    'SystemCallError')
      RUBY.global('ThreadError',    'ThreadError')
      RUBY.global('TypeError',  'TypeError')

    RUBY.global('NameError',    'NameError')
      RUBY.global('NoMethodError',  'MessageNotUnderstood')
    RUBY.global('ZeroDivisionError', 'ZeroDivide')



# ----------------------------------------------------------
#  classes related to implementation of Proc, not extendable by Ruby

RUBY.global("ExecBlock", "ExecBlock")
RUBY.global("VariableContext", "VariableContext")
RUBY.global("GsNMethod", "GsNMethod")

# ----------------------------------------------------------

# ARGV initialized in .mcz, in RubyTransientConstantAssociation>>initGlobals:
ARGF = nil # TODO a real implementation

# Notes on various globals
begin
  #  $/ is auto-initialized to "\n" by parser at first ref .
  x = $/
  #  $; is auto-initialized to nil if referenced, by RubySexpParser .
  y = $;
end

#  $!  translated to rescue block block-arg-ref  or to the global $!
#    by RubyGlobalVarNode and RubyRescueBodyNode

#  $&  $_  $` $' $1..$9 $~  all translated to access to
#    appropriate frame-local data associated with $~

# $: , RUBY
#  are currently initialized in RubyContext(C)>>initialize

alias $-0  $/
alias $-F  $;
alias $-d $DEBUG
alias $-I $:
alias $-v $VERBOSE
alias $-w $VERBOSE

alias $LOADED_FEATURES $"
alias $LOAD_PATH  $:
alias $<  $ARGF
alias $*  $ARGF

# -------------------

# RUBY_ENGINE is proposed as a way of distinguishing between JRuby, MRI,
# YARV, IronRuby, MagLev etc.  While it is not official, it has support
# from other implementers, wo we include it here.  It is also useful to
# distinguish scripts running in MRI, since MRI 1.8.6 doesn't define it.
RUBY_ENGINE = 'maglev'

# A quick hack to get specs further w/o build having to poke the right string in
#  this slot. See ticket #76
RUBY_PLATFORM = 'Unspecified.platform'
RUBY_VERSION = '1.8.6'
# Note - the packager modifies the date on any line starting with RUBY_RELEASE_DATE
RUBY_RELEASE_DATE = '09/15/2008'
RUBY_PATCHLEVEL = 114  # TODO: this is what MRI 1.8.6 shows...

VERSION = RUBY_VERSION
# If DEBUG_SPEC is true an executing rubyspec pauses on exception for topaz debugging
# If false, the handling is unchanged: exception is reported along with the
#  stringified stack.
# Value is examined by mspec/runner/mspec.rb.
DEBUG_SPEC = false

TRUE  = true
FALSE = false
NIL = nil
#  associationsfor TRUE, FALSE, NIL frozen by RubyContext>>_requirePrimitives...
#
# do not initialize STDOUT , STDIN, STDERR , they are handled by the
#   transient context state initialization
