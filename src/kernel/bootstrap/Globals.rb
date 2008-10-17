# We define all of these globals up front so that we can open these classes later without accidentally creating new ones

#            Ruby class name , Gemstone class name
RUBY.global('Array', 'Array')
RUBY.global('Behavior', 'Behavior')
RUBY.global('Boolean', 'Boolean')
RUBY.global('Class', 'Class')
RUBY.global('FalseClass', 'FalseClass')
RUBY.global('File', 'GsFile')
RUBY.global('File::Stat', 'GsFileStat')  # File::Stat -->  GsFileStat
RUBY.global('Fixnum', 'SmallInteger')
RUBY.global('Float', 'Float')
RUBY.global('Hash', 'RubyHash')
RUBY.global('Integer', 'Integer')
RUBY.global('IO', 'IO')
RUBY.global('IPSocket', 'IPSocket')
RUBY.global('Kernel', 'Kernel')    # for module Kernel
RUBY.global('MatchData', 'MatchData')
RUBY.global('Method', 'RubyMethod')  # RubyMethod , RubyUnboundMethod defined in .mcz
RUBY.global('Module', 'Module')    # for class Module
RUBY.global('NilClass', 'UndefinedObject')
RUBY.global('Numeric', 'Number')
RUBY.global('Object', 'Object')
RUBY.global('Random')
RUBY.global('Range', 'Range')
RUBY.global('Regexp', 'Regexp')
RUBY.global('Set', 'IdentitySet')
RUBY.global('Socket', 'Socket')
RUBY.global('String', 'String')
RUBY.global('Symbol', 'Symbol')
RUBY.global('TCPServer', 'TCPServer')
RUBY.global('TCPSocket', 'TCPSocket')
RUBY.global('Thread', 'GsProcess')
RUBY.global('Time', 'RubyTime')
RUBY.global('TrueClass', 'TrueClass')
RUBY.global('UnboundMethod', 'RubyUnboundMethod')

RUBY.global('Gemstone', 'System')

#
#  Define some of the Ruby exception classes to be identical to certain
#   Smalltalk exception classes   .
#  see GlobalErrors.rb for additional creation of Ruby exception classes
#
#            Ruby name           Smalltalk Name
#
RUBY.global('Exception',    'UserException')
  RUBY.global('SystemExit',     'RubySystemExit')
  RUBY.global('SystemStackExit',    'StackOverflow')
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
          RUBY.global('EBADF' ,         'SocketErrorEBADF')
          RUBY.global('ENOTCONN' ,  'SocketErrorENOTCONN')
          RUBY.global('EPIPE' ,         'SocketErrorEPIPE')
          RUBY.global('ECONNRESET' ,    'SocketErrorECONNRESET')
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

RUBY.global("Proc", "ExecBlock")
def proc(&b); b; end
# remainder of Proc support in Proc.rb

# ----------------------------------------------------------

ARGV = []

# Notes on various globals
#  $; is auto-initialized to nil if referenced, by RubySexpParser .
#  $/ is auto-initialized to "\n" by parser at first ref .
#  $-0 is translated to $/ by parser .
#  $-F is translated to  $;  by parser .

#  $!  translated to exception block block-arg-ref by RubyGlobalVarNode
#         and RubyRescueBodyNode
#  $&  $_  $` $' $1..$9 $~  all translated to access to thread-local
#    data related to $~

# -------------------

# A quick hack to get specs further w/o build having to poke the right string in
#  this slot. See ticket #76
RUBY_PLATFORM = 'Unspecified.platform'
RUBY_VERSION = '1.8.6'
RUBY_RELEASE_DATE = '09/15/2008' #TODO: Date should be inserted by build framework
RUBY_PATCHLEVEL = '114'  # TODO: this is what MRI 1.8.6 shows...

# If DEBUG_SPEC is true an executing rubyspec pauses on exception for topaz debugging
# If false, the handling is unchanged: exception is reported along with the
#  stringified stack.
# Value is examined by mspec/runner/mspec.rb.
DEBUG_SPEC = false

TRUE  = true
FALSE = false
